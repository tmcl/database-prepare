{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Map qualified
import Database.PostgreSQL.LibPQ
import Database.Postgres.Temp
import Database.Prepare.Postgresql.FromField
import Database.Prepare.Postgresql.GetInfo
import Database.Prepare.Postgresql.TH
import Control.Exception
import System.Exit

main :: IO ()
main = do
  results <- sequence
    [ run "NoResultError on bad connection" testNoResultError
    , run "ResultError on invalid SQL" testResultError
    , run "ColumnMismatch on wrong expectedFields" testColumnMismatch
    , run "parseOrDie throws on Left" testParseOrDie
    ]
  if and results
    then Prelude.putStrLn "All tests passed."
    else exitFailure

run :: String -> IO Bool -> IO Bool
run label test = do
  Prelude.putStr (label <> "... ")
  result <- test `catch` \(e :: SomeException) -> do
    Prelude.putStrLn ("EXCEPTION: " <> Prelude.show e)
    pure False
  Prelude.putStrLn (if result then "PASS" else "FAIL")
  pure result

-- A closed/invalid connection should yield NoResultError
testNoResultError :: IO Bool
testNoResultError = do
  conn <- connectdb ""
  finish conn
  result <- executeQuery conn "select 1" Database.PostgreSQL.LibPQ.Text [] Data.Map.empty (\_ _ -> pure ())
  pure $ case result of
    Left (NoResultError _) -> True
    _ -> False

-- Executing SQL that references a nonexistent table should yield ResultError
testResultError :: IO Bool
testResultError = withResult \conn -> do
  result <- executeQuery conn "select * from nonexistent_table_xyz" Database.PostgreSQL.LibPQ.Text [] Data.Map.empty (\_ _ -> pure ())
  pure $ case result of
    Left (ResultError _ _) -> True
    _ -> False

-- Passing expectedFields that don't match the actual query result should yield SchemaIssue ColumnMismatch
testColumnMismatch :: IO Bool
testColumnMismatch = withResult \conn -> do
  _ <- exec conn "create table if not exists test_mismatch (id integer primary key, name text not null)"
  let wrongFields = Data.Map.fromList
        [ (0, ComparableColumnInfo 0 "text" (Just "wrong_col"))
        , (1, ComparableColumnInfo 1 "int4" (Just "also_wrong"))
        ]
  result <- executeQuery conn "select id, name from test_mismatch" Database.PostgreSQL.LibPQ.Text [] wrongFields (\_ _ -> pure ())
  pure $ case result of
    Left (SchemaIssue (ColumnMismatch _ _)) -> True
    _ -> False

-- parseOrDie should throw an error when given Left
testParseOrDie :: IO Bool
testParseOrDie = do
  threw <- try @SomeException (evaluate (parseOrDie "test context" (Left NoParse :: Either ParserError Int)))
  pure $ case threw of
    Left _ -> True
    Right _ -> False

withResult :: (Connection -> IO a) -> IO a
withResult f = with (\pgdb -> do
  conn <- connectdb (toConnectionString pgdb)
  f conn) >>= \case
    Left err -> fail (Prelude.show err)
    Right a -> pure a

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BasicNamed
import Control.Monad
import Data.Row.Records
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Ok
import InsertNamed
import SqliteTH

$(embedSqlite ["test-data/schema/schema.sql"] "test-data/schema/schema.sql" "buildSchema")

main :: IO ()
main = do
  conn <- open "test-data/test.db"
  buildSchema conn
  ixes <- insertNamed conn $ InsertNamedParams (Label @"$name" .== SQLText "Alice" .+ Label @"$email" .== SQLText "olooe@aoice.example" .+ Data.Row.Records.empty)
  forM_ ixes \(InsertNamedResult ix) -> do
    let qp = BasicNamed.QueryParams {qpEmail = Nothing, qpId = ok2maybe (fromField $ ix .! #id)}
    basicNamed conn qp
      >>= print
  pure ()

ok2maybe :: Ok a -> Maybe a
ok2maybe = \case
  Ok a -> Just a
  _ -> Nothing

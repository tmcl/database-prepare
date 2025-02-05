{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import SqliteTH
import Data.Row.Records
import Database.SQLite.Simple
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.FromField
import BasicNamed
import InsertNamed
import Control.Monad

$( embedSqlite ["test-data/schema/schema.sql"] "test-data/schema/schema.sql" "buildSchema" )

main :: IO ()
main = do
  conn <- open "test-data/test.db"
  buildSchema conn
  ixes <- insertNamed conn $ InsertNamed.Params ( Label @"$name" .== SQLText "Alice" .+ Label @"$email" .== SQLText "olooe@aoice.example" .+ Data.Row.Records.empty )
  forM_ ixes \(InsertNamed.Result ix) -> do
    let qp = BasicNamed.QueryParams { qpEmail = Nothing, qpId = ok2maybe (fromField $ ix .! #id) }
    basicNamed conn qp
     >>= print
  pure ()

ok2maybe :: Ok a -> Maybe a
ok2maybe = \case
  Ok a -> Just a
  _ -> Nothing

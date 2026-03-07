{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import BasicParams
import BasicNoparams
import DeleteAll
import DeleteParams
import InsertParams
import MappedTypes
import Database.Postgres.Temp
import Data.ByteString
import Database.PostgreSQL.LibPQ

main :: IO()
main = () <$ with \pgdb -> do
      conn <- connectdb (toConnectionString pgdb)
      schemaSql <- Data.ByteString.readFile "test-data/schema/schema.sql"
      schema <- exec conn schemaSql

      case schema of
        Nothing -> fail "could not use db connection"
        Just f -> do
           (resultErrorMessage f)
             >>= \case
               Nothing -> pure ()
               Just "" -> pure ()
               Just err -> fail . Prelude.show $ err
      print =<< insertUser conn UserParams { email = "alice@email.example", name = "Alice" }
      print =<< queryUsers conn
      print =<< searchUsers conn (UserSearch 1 "alice@email.example")
      print =<< deleteUser conn DeleteUserParams { dupId = 1 }
      print =<< deleteAll conn

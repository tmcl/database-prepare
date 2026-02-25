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

import BasicNamed
import BasicNoparams
import DeleteAll
import DeleteNamed
import InsertNamed
import Database.Postgres.Temp
import Data.ByteString
import Database.PostgreSQL.LibPQ
import Data.Tuple

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
      print =<< basicNamed conn 1 "alice@email.example"
      print =<< queryUsers conn
      print =<< deleteUser conn (DeleteUserParams (MkSolo 1))
      print =<< deleteAll conn

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -ddump-splices #-}

module DeleteParams (deleteUser, DeleteUserParams(..)) where

import Data.ByteString
import Data.Int
import Database.PostgreSQL.LibPQ
import Database.Postgres.Temp
import Database.Prepare.Postgresql.TH
import Language.Haskell.TH

data DeleteUserParams = DeleteUserParams { dupId :: Int32 }
  deriving (Show)

$(return [])

deleteUser :: Connection -> DeleteUserParams -> IO (Either PostgresError ())
deleteUser = $(do
    Right pgdb <- runIO $ startConfig defaultConfig
    conn <- runIO $ connectdb (toConnectionString pgdb)
    schemaSql <- runIO $ Data.ByteString.readFile "test-data/schema/schema.sql"
    schema <- runIO $ exec conn schemaSql

    case schema of
      Nothing -> fail "could not use db connection"
      Just f -> do
         runIO (resultErrorMessage f)
           >>= \case
             Nothing -> pure ()
             Just "" -> pure ()
             Just err -> fail . Prelude.show $ err
    f <- embedPostgres conn "test-data/query/delete-named.sql"
      (Just (''DeleteUserParams, [("dupId", "$1")]))
      Nothing
    runIO $ finish conn
    runIO $ stop pgdb
    pure f
  )

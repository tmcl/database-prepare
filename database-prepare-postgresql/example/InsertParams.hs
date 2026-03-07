{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -ddump-splices #-}

module InsertParams (insertUser, UserParams(..)) where

import Data.ByteString
import Data.Int
import Data.Text
import Database.PostgreSQL.LibPQ
import Database.Postgres.Temp
import Database.Prepare.Postgresql.TH
import Language.Haskell.TH

data UserParams = UserParams { name :: Text, email :: Text }
  deriving (Show)

newtype UserId = UserId Int32
  deriving (Show)

data InsertResult = InsertResult { irId :: Int32 }
  deriving (Show)

$(return [])

insertImpl :: Connection -> UserParams -> IO (Either PostgresError [InsertResult])
insertImpl = $(do
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
    f <- embedPostgres conn "test-data/query/insert-named.sql"
      (Just (''UserParams, [("name", "$1"), ("email", "$2")]))
      (Just (''InsertResult, [("irId", "id")]))
    runIO $ finish conn
    runIO $ stop pgdb
    pure f
  )

insertUser :: Connection -> UserParams -> IO (Either PostgresError [UserId])
insertUser conn params = fmap (fmap (UserId . irId)) <$> insertImpl conn params

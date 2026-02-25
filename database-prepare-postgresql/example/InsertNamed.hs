{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -ddump-splices #-}

module InsertNamed (insertUser, UserParams(..)) where

import Database.PostgreSQL.LibPQ
import Database.Postgres.Temp
import Database.Prepare.Postgresql.TH
import Data.ByteString
import Data.Int
import Data.Row
import Data.Text
import Language.Haskell.TH

$( do
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
      f <- embedPostgres conn "test-data/query/insert-named.sql" "insertNamed"
      runIO $ finish conn
      runIO $ stop pgdb
      pure f
  )

newtype UserId = UserId Int32
 deriving (Show)
data UserParams = UserParams { name :: Text, email :: Text }
 deriving (Show)

insertUser :: Connection -> UserParams -> IO (Either PostgresError [UserId])
insertUser conn params = do
  let insertNamedParams = InsertNamedParams (params.name, params.email)
  ei <- insertNamed conn insertNamedParams
  case ei of
    Left err -> pure (Left err)
    Right several -> pure . Right $ (\(InsertNamedResult inr) -> UserId $ inr .! #id) <$> several

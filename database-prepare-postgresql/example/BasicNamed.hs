{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -ddump-splices #-}

module BasicNamed (basicNamed, BasicNamedQuery(..)) where

import Data.ByteString
import Data.Int
import Data.Row
import Data.Row.Records
import Data.Text
import Database.PostgreSQL.LibPQ
import Database.Postgres.Temp
import Database.Prepare.Postgresql.TH
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
      f <- embedPostgres conn "test-data/query/basic-named.sql" "basicNamedQuery"
      runIO $ finish conn
      runIO $ stop pgdb
      pure f
  )

data BasicNamedQuery = BasicNamedQuery
  { bId :: Int32
  , bName :: Text
  , bEmail :: Text
  } deriving (Show)

basicNamed :: Connection -> Int32 -> Text -> IO (Either PostgresError [BasicNamedQuery])
basicNamed conn idParam emailParam = do
  let params = BasicNamedQueryParams (idParam, emailParam)
  ei <- basicNamedQuery conn params
  case ei of
    Left err -> pure (Left err)
    Right several -> pure . Right $ processResultRow <$> several
  where
   processResultRow (BasicNamedQueryResult qar) = BasicNamedQuery {..}
     where
       bId = qar .! #id
       bName = qar .! #name
       bEmail = qar .! #email

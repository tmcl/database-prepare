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


module BasicNoparams(queryUsers) where

import Data.Row.Records

import Database.Prepare.Postgresql.TH
import Language.Haskell.TH
import Database.PostgreSQL.LibPQ
import Database.Postgres.Temp
import Data.Int
import Data.Text
import Data.Time

$( do
      Right pgdb <- runIO $ startConfig defaultConfig
      conn <- runIO $ connectdb (toConnectionString pgdb)
      schema <- runIO $ exec conn "\
        \ create table if not exists users ( \
        \   id integer primary key not null, \
        \   name text not null, \
        \   email text not null unique, \
        \   created_at timestamptz not null default (now()) \
        \ );"

      case schema of
        Nothing -> fail "could not use db connection"
        Just f -> do
           runIO (resultErrorMessage f)
             >>= \case
               Nothing -> pure ()
               Just "" -> pure ()
               Just err -> fail . Prelude.show $ err
      f <- embedPostgres conn "test-data/query/basic-noparams.sql" "queryAll"
      runIO $ finish conn
      runIO $ stop pgdb
      pure f
  )

newtype UserId = UserId Int32
  deriving (Show)
data User = User
  { uId::UserId
  , uName::Text
  , uEmail::Text
  , uCreatedAt::UTCTime
  } deriving (Show)

queryUsers :: Connection -> IO (Either PostgresError [User])
queryUsers conn = do
  ei <- queryAll conn
  case ei of
    Left err -> pure (Left err)
    Right several -> pure . Right $ processResultRow <$> several
  where
   processResultRow (QueryAllResult qar) = User {..}
     where
       uId  = UserId $ qar .! #id
       uName = qar .! #name
       uEmail = qar .! #email
       uCreatedAt = qar .! #created_at

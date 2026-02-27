{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -ddump-splices #-}

module BasicNamed where

import Data.Row.Records
import Data.Text
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Database.Prepare.Sqlite.TH

$(embedSqlite "test-data/schema" "test-data/query/basic-named.sql" "queryAll")
$(embedSqlite "test-data/schema" "test-data/query/basic-named.sql" "distraction")

data User = User {uId :: Int, name :: Text, email :: Text, created :: UTCTime}
  deriving (Show, Eq)

data QueryParams = QueryParams {qpEmail :: Maybe Text, qpId :: Maybe Int}
  deriving (Show, Eq)

qp2params :: QueryParams -> QueryAllParams
qp2params QueryParams {..} = QueryAllParams (Label @"$email" .== toField qpEmail .+ Label @"$id" .== toField qpId .+ Data.Row.Records.empty)

result2User :: QueryAllResult -> Ok User
result2User (QueryAllResult result) = do
  uId <- fromField $ result .! #id
  name <- fromField $ result .! #name
  email <- fromField $ result .! #email
  created <- fromField $ result .! #created_at

  pure User {..}

basicNamed :: Connection -> QueryParams -> IO (Ok [User])
basicNamed conn = fmap (mapM result2User) . queryAll conn . qp2params

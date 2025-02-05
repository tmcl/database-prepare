{-# LANGUAGE TemplateHaskell #-}
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


module BasicNamed where

import SqliteTH
import Data.Row.Records
import Data.Text
import Database.SQLite.Simple
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Data.Time

$(embedSqlite ["test-data/schema/schema.sql"] "test-data/query/basic-named.sql" "queryAll" )

data User = User { uId :: Int, name :: Text, email :: Text, created :: UTCTime }
  deriving (Show, Eq)

data QueryParams = QueryParams { qpEmail :: Maybe Text, qpId :: Maybe Int }
  deriving (Show, Eq)


qp2params :: QueryParams -> Params
qp2params QueryParams {..} = Params ( Label @"$email" .== toField qpEmail .+ Label @"$id" .== toField qpId .+ Data.Row.Records.empty )

result2User :: Result -> Ok User
result2User (Result result) = do
                               uId <- fromField $ result .! #id
                               name <- fromField $ result .! #name
                               email <- fromField $ result .! #email
                               created <- fromField $ result .! #created_at

                               pure User{..}

basicNamed :: Connection -> QueryParams -> IO (Ok [User])
basicNamed conn = fmap (mapM result2User) . queryAll conn . qp2params



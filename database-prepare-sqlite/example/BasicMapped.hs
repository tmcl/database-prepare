{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -ddump-splices #-}

module BasicMapped (searchUsers) where

import Database.SQLite.Simple
import Database.Prepare.Sqlite.TH
import MappedTypes

searchUsers :: Connection -> UserSearch -> IO [User]
searchUsers = $(embedSqliteMapped
    "test-data/schema"
    "test-data/query/basic-named.sql"
    (Just (''UserSearch, fieldPairs userSearchParamMap))
    (Just (''User, fieldPairs userResultMap))
  )

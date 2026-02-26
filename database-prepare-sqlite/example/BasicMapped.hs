{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -ddump-splices -Wno-orphans #-}

module BasicMapped (searchUsers) where

import Database.Prepare.Sqlite.TH
import MappedTypes

$(embedSqliteMapped
    ["test-data/schema/schema.sql"]
    "test-data/query/basic-named.sql"
    "searchUsers"
    (Just (''UserSearch, fieldPairs userSearchParamMap))
    (Just (''User, fieldPairs userResultMap))
  )

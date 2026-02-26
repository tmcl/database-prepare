{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module MappedTypes where

import Data.Text
import Data.Time
import Database.Prepare.Sqlite.TH
import GHC.Generics qualified

data User = User
  { uId :: Int
  , uName :: Text
  , uEmail :: Text
  , uCreatedAt :: UTCTime
  } deriving (Show, GHC.Generics.Generic)

data UserSearch = UserSearch
  { usId :: Maybe Int
  , usEmail :: Maybe Text
  } deriving (Show, GHC.Generics.Generic)

$(deriveMappingType ''User)
$(deriveMappingType ''UserSearch)

userResultMap :: UserFieldNames
userResultMap = UserFieldNames
  { uId = "id"
  , uName = "name"
  , uEmail = "email"
  , uCreatedAt = "created_at"
  }

userSearchParamMap :: UserSearchFieldNames
userSearchParamMap = UserSearchFieldNames
  { usId = "$id"
  , usEmail = "$email"
  }

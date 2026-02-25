{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Database.Prepare.Postgresql.ToField where

import Data.Text
import Data.Text.Encoding
import Database.PostgreSQL.LibPQ
import Data.ByteString
import Data.Proxy
import GHC.TypeLits
import Database.Prepare.Postgresql.PgType

class ToField (pgTypeName::Symbol) where
  toField :: Proxy pgTypeName -> PgType pgTypeName -> (ByteString, Format)
instance ToField "text" where
  toField :: Proxy "text" -> Text -> (ByteString, Format)
  toField _ val = (encodeUtf8 val, Database.PostgreSQL.LibPQ.Text)

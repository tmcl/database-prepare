{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Database.Prepare.Postgresql.ToField where

import Data.ByteString
import Data.Int
import Data.Proxy
import Data.Text
import Data.Text.Encoding
import Database.PostgreSQL.LibPQ
import Database.Prepare.Postgresql.PgType
import GHC.TypeLits

class ToField (pgTypeName::Symbol) where
  toField :: Proxy pgTypeName -> PgType pgTypeName -> (ByteString, Format)
instance ToField "text" where
  toField :: Proxy "text" -> Text -> (ByteString, Format)
  toField _ val = (encodeUtf8 val, Database.PostgreSQL.LibPQ.Text)
instance ToField "int4" where
  toField :: Proxy "int4" -> Int32 -> (ByteString, Format)
  toField _ val = (encodeUtf8 (Data.Text.pack (Prelude.show val)), Database.PostgreSQL.LibPQ.Text)

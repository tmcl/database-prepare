{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Database.Prepare.Postgresql.ToField where

import Data.Kind
import Data.Text
import Data.Text.Encoding
import Database.PostgreSQL.LibPQ
import Data.ByteString
import Data.Proxy
import GHC.TypeLits

type family PgParam (oid::Symbol) :: Data.Kind.Type

class ToField (oid::Symbol) where
  toField :: Proxy oid -> PgParam oid -> (Oid, ByteString, Format)

type instance PgParam "text" = Data.Text.Text
instance ToField "text" where
  toField :: Proxy "text" -> Text -> (Oid, ByteString, Format)
  toField _ val = (Oid 25, encodeUtf8 val, Database.PostgreSQL.LibPQ.Text)

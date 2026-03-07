{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Prepare.Postgresql.ToField where

import Data.ByteString
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Int
import Data.Proxy
import Data.Text
import Data.Text.Encoding
import Data.Time
import Database.PostgreSQL.LibPQ
import GHC.TypeLits

class ToField (pgTypeName::Symbol) ty where
  toField :: Proxy pgTypeName -> ty -> (ByteString, Format)
instance ToField "text" Text where
  toField :: Proxy "text" -> Text -> (ByteString, Format)
  toField _ val = (encodeUtf8 val, Database.PostgreSQL.LibPQ.Text)
instance ToField "int4" Int32 where
  toField :: Proxy "int4" -> Int32 -> (ByteString, Format)
  toField _ val = (encodeUtf8 (Data.Text.pack (Prelude.show val)), Database.PostgreSQL.LibPQ.Text)
instance ToField "int8" Int64 where
  toField :: Proxy "int8" -> Int64 -> (ByteString, Format)
  toField _ val = (encodeUtf8 (Data.Text.pack (Prelude.show val)), Database.PostgreSQL.LibPQ.Text)
instance ToField "timestamptz" UTCTime where
  toField _ val = (encodeUtf8 (Data.Text.pack (formatTime defaultTimeLocale "%F %T%Q+00" val)), Database.PostgreSQL.LibPQ.Text)
instance ToField "bool" Bool where
  toField _ val = (if val then "t" else "f", Database.PostgreSQL.LibPQ.Text)
instance ToField "bytea" ByteString where
  toField _ val = ("\\x" <> LBS.toStrict (Builder.toLazyByteString (BS.foldl' (\b w -> b <> Builder.word8HexFixed w) mempty val)), Database.PostgreSQL.LibPQ.Text)
instance ToField "varchar" Text where
  toField _ val = (encodeUtf8 val, Database.PostgreSQL.LibPQ.Text)
instance ToField "json" ByteString where
  toField _ val = (val, Database.PostgreSQL.LibPQ.Text)
instance ToField "float8" Double where
  toField _ val = (encodeUtf8 (Data.Text.pack (Prelude.show val)), Database.PostgreSQL.LibPQ.Text)

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Database.Prepare.Postgresql.FromField where

import Data.Kind
import Data.Text
import Data.Text.Encoding
import Database.PostgreSQL.LibPQ
import Data.ByteString
import Data.Proxy
import GHC.TypeLits
import Data.Text.Encoding.Error
import Data.Bifunctor
import Data.Int
import Text.Read
import Data.Time

type family PgParam (oid::Symbol) :: Data.Kind.Type

data ParserError = Utf8Exception UnicodeException | Unimplemented | NoParse
  deriving (Show)

class FromField (oid::Symbol) where
  fromField :: Proxy oid -> Format -> ByteString -> Either ParserError (PgParam oid)

type instance PgParam "int4" = Int32
type instance PgParam "text" = Data.Text.Text
type instance PgParam "timestamptz" = UTCTime

builtinOids :: Natural -> Maybe String
builtinOids = \case
  23 -> Just "int4"
  25 -> Just "text"
  1184 -> Just "timestamptz"
  _ -> Nothing

instance FromField "timestamptz" where
  fromField :: Proxy "timestamptz" -> Format -> ByteString -> Either ParserError UTCTime
  fromField _ Database.PostgreSQL.LibPQ.Text bs = case decodeUtf8' bs of
      Left err -> Left $ Utf8Exception err
      Right txt -> case readMaybe (Data.Text.unpack txt) of
         Just r -> Right r
         Nothing -> Left NoParse
  fromField _ Database.PostgreSQL.LibPQ.Binary _bs = Left Unimplemented

instance FromField "int4" where
  fromField :: Proxy "int4" -> Format -> ByteString -> Either ParserError Int32
  fromField _ Database.PostgreSQL.LibPQ.Text bs = case decodeUtf8' bs of
      Left err -> Left $ Utf8Exception err
      Right txt -> case readMaybe (Data.Text.unpack txt) of
         Just r -> Right r
         Nothing -> Left NoParse
  fromField _ Database.PostgreSQL.LibPQ.Binary _bs = Left Unimplemented

instance FromField "text" where
  fromField :: Proxy "text" -> Format -> ByteString -> Either ParserError Data.Text.Text
  fromField _ Database.PostgreSQL.LibPQ.Text bs = first Utf8Exception $ decodeUtf8' bs
  fromField _ Database.PostgreSQL.LibPQ.Binary _bs = Left Unimplemented

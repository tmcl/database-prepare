{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Database.Prepare.Postgresql.FromField where

import Data.Attoparsec.ByteString.Char8 qualified
import Data.Bifunctor
import Data.ByteString
import Data.Int
import Data.Proxy
import Data.Text
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Time
import Database.PostgreSQL.LibPQ
import Database.Prepare.Postgresql.PgType
import Database.Prepare.Postgresql.Time.Parser qualified
import GHC.TypeLits
import Text.Read

data ParserError = Utf8Exception UnicodeException | Unimplemented | NoParse
  deriving (Show)

class FromField (pgTypeName::Symbol) where
  fromField :: Proxy pgTypeName -> Format -> ByteString -> Either ParserError (PgType pgTypeName)

instance FromField "timestamptz" where
  fromField :: Proxy "timestamptz" -> Format -> ByteString -> Either ParserError UTCTime
  fromField _ Database.PostgreSQL.LibPQ.Text bs =
      case Data.Attoparsec.ByteString.Char8.parseOnly Database.Prepare.Postgresql.Time.Parser.utcTime bs of
        Right r -> Right r
        Left _ -> Left NoParse
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

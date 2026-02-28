{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Database.Prepare.Postgresql.FromField where

import Data.Attoparsec.ByteString.Char8 qualified
import Data.Bifunctor
import Data.Bits
import Data.ByteString
import Data.ByteString qualified as BS
import Data.Int
import Data.Proxy
import Data.Text
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Time
import Data.Word
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

instance FromField "int8" where
  fromField :: Proxy "int8" -> Format -> ByteString -> Either ParserError Int64
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

instance FromField "bool" where
  fromField _ Database.PostgreSQL.LibPQ.Text bs = case bs of
    "t" -> Right True
    "f" -> Right False
    _ -> Left NoParse
  fromField _ Database.PostgreSQL.LibPQ.Binary _bs = Left Unimplemented

instance FromField "bytea" where
  fromField _ Database.PostgreSQL.LibPQ.Text bs = case BS.stripPrefix "\\x" bs of
    Just hexBs -> decodeHex hexBs
    Nothing -> Left NoParse
    where
      decodeHex hex
        | BS.null hex = Right BS.empty
        | BS.length hex < 2 = Left NoParse
        | otherwise = case (,) <$> fromHexNibble (BS.index hex 0) <*> fromHexNibble (BS.index hex 1) of
            Nothing -> Left NoParse
            Just (hi, lo) -> case decodeHex (BS.drop 2 hex) of
              Left e -> Left e
              Right rest -> Right (BS.cons (shiftL hi 4 .|. lo) rest)
      fromHexNibble :: Word8 -> Maybe Word8
      fromHexNibble w
        | w >= 0x30 && w <= 0x39 = Just (w - 0x30)
        | w >= 0x41 && w <= 0x46 = Just (w - 0x37)
        | w >= 0x61 && w <= 0x66 = Just (w - 0x57)
        | otherwise = Nothing
  fromField _ Database.PostgreSQL.LibPQ.Binary _bs = Left Unimplemented

instance FromField "varchar" where
  fromField _ Database.PostgreSQL.LibPQ.Text bs = first Utf8Exception $ decodeUtf8' bs
  fromField _ Database.PostgreSQL.LibPQ.Binary _bs = Left Unimplemented

instance FromField "float8" where
  fromField _ Database.PostgreSQL.LibPQ.Text bs = case decodeUtf8' bs of
      Left err -> Left $ Utf8Exception err
      Right txt -> case readMaybe (Data.Text.unpack txt) of
         Just r -> Right r
         Nothing -> Left NoParse
  fromField _ Database.PostgreSQL.LibPQ.Binary _bs = Left Unimplemented

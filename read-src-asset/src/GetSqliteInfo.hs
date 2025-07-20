{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

module GetSqliteInfo (continueWith, SqliteStatement(..)) where

import Data.Function
import GHC.Stack
import Effectful.Exception
import Data.Bifunctor
import Effectful
import Effectful.Error.Static
import MigrateSchema
import Database.SQLite3.Direct
import Data.Text.Encoding
import Data.ByteString
import Control.Monad
import Data.Maybe
import Filesystem.Path.CurrentOS
import Data.Set
import Data.Text.Encoding.Error (UnicodeException)

data Arguments = Arguments
  { out :: Maybe Filesystem.Path.CurrentOS.FilePath
  , paths :: Data.Set.Set Filesystem.Path.CurrentOS.FilePath
  , argSchemaPaths :: Data.Set.Set Filesystem.Path.CurrentOS.FilePath
  } deriving (Show)


data SqliteStatement = JustSql Data.ByteString.ByteString | SqliteStatement
  { ssfp :: Filesystem.Path.CurrentOS.FilePath
  , sssql :: Utf8
  , ssParamNames :: [(ParamIndex, Maybe Utf8)]
  , ssResultNames :: [(ColumnIndex, Utf8)]
  } deriving (Show)

data GetSqlInfoError = GetSqlInfoError { sourceFile :: Filesystem.Path.CurrentOS.FilePath,
   sourceBytestring:: Data.ByteString.ByteString, actualError :: GetSqlInfoClassifiedError }
 deriving (Show)

data GetSqlInfoClassifiedError =
 FatalError FatalError | NonfatalError NonfatalError
 deriving (Show)

data FatalError = InvalidUtf8 UnicodeException | FatalDirectSqlError DirectSqlError
 deriving (Show)
data NonfatalError = NonfatalDirectSqlError DirectSqlError Database.SQLite3.Direct.Error Utf8 | EmptyFile
 deriving (Show)

wrapFatal :: DirectSqlError -> GetSqlInfoError
wrapFatal = GetSqlInfoError "" "" . FatalError . FatalDirectSqlError

continueWith :: (HasCallStack) => Data.Set.Set Filesystem.Path.CurrentOS.FilePath  -> Filesystem.Path.CurrentOS.FilePath -> IO (Either (CallStack, GetSqlInfoError) SqliteStatement)
continueWith schemaPaths queryPath = do
        if Data.Set.member queryPath schemaPaths
          then basicEncode
          else Effectful.runEff (runError applySchema)

   where
    finalizeIfNeeded = \case
                 Right (Just stmt) -> do
                   void $ liftIO $ finalize stmt
                 _ -> pure ()

    applySchema ::  Eff [Effectful.Error.Static.Error GetSqlInfoError, IOE] SqliteStatement
    applySchema = do
       let path = encodeString queryPath
       sqlBs <- liftIO $ Data.ByteString.readFile path
       sql <- (fmap encodeUtf8 . decodeUtf8') sqlBs
         & \case
              Left e -> throwError $ GetSqlInfoError queryPath sqlBs $ FatalError $ InvalidUtf8 e
              Right sql -> pure $ Utf8 sql


       bracket (liftEIO (first wrapFatal <$> open ":memory:")) (liftEIO . fmap (first (wrapFatal . (\e -> (e, "close") :: DirectSqlError))) . close) \db -> do
           liftEIO (first wrapFatal <$> exec db "begin transaction")
           migrateSchema wrapFatal db (Data.Set.toList schemaPaths)
           liftEIO (first wrapFatal <$> exec db "commit")
           bracket (liftIO ((first \e -> (e, sql)) <$> prepare db sql)) finalizeIfNeeded \emstmt -> do
               case emstmt of
                 Right (Just stmt) -> do
                   paramsCardinality <- liftIO $ bindParameterCount stmt
                   paramNames <- forM [1..paramsCardinality ] \ix -> do
                     paramName <- liftIO $ bindParameterName stmt ix
                     pure (ix, paramName)
                   numCols <- liftIO $ columnCount stmt
                   colNames <- forM [0..numCols - 1] \ix -> do
                     colName <- liftIO $ columnName stmt ix
                     pure (ix, colName)
                   pure $ SqliteStatement {sssql = sql  , ssfp = queryPath, ssParamNames = paramNames, ssResultNames = mapMaybe sequence colNames }
                 Right Nothing -> throwError $ GetSqlInfoError queryPath sqlBs $ NonfatalError EmptyFile
                 Left e -> do 
                   dse <- liftIO $ extendedErrcode db
                   dsm <- liftIO $ errmsg db
                   throwError $ GetSqlInfoError queryPath sqlBs $ NonfatalError $ NonfatalDirectSqlError e dse dsm

    basicEncode :: HasCallStack => IO (Either (CallStack, GetSqlInfoError) SqliteStatement)
    basicEncode = do
        let str = Filesystem.Path.CurrentOS.encodeString queryPath
        sqlBs <- Data.ByteString.readFile str
        let sql = (fmap encodeUtf8 . decodeUtf8') sqlBs
        pure (bimap (\x -> (callStack, GetSqlInfoError queryPath sqlBs . FatalError . InvalidUtf8 $ x))  JustSql sql)


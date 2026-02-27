{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Prepare.Sqlite.GetInfo (continueWith, SqliteStatement (..)) where

import Control.Monad
import Data.Bifunctor
import Data.ByteString
import Data.Function
import Data.Maybe
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Database.SQLite3.Direct
import Effectful
import Effectful.Error.Static
import Effectful.Exception
import GHC.Stack
import Database.Prepare.Sqlite.MigrateSchema

data SqliteStatement = SqliteStatement
  { ssfp :: FilePath,
    sssql :: Utf8,
    ssParamNames :: [(ParamIndex, Maybe Utf8)],
    ssResultNames :: [(ColumnIndex, Utf8)]
  }
  deriving (Show)

data GetSqlInfoError = GetSqlInfoError
  { sourceFile :: FilePath,
    sourceBytestring :: Data.ByteString.ByteString,
    actualError :: GetSqlInfoClassifiedError
  }
  deriving (Show)

data GetSqlInfoClassifiedError
  = FatalError FatalError
  | NonfatalError NonfatalError
  deriving (Show)

data FatalError = InvalidUtf8 UnicodeException | FatalDirectSqlError DirectSqlError
  deriving (Show)

data NonfatalError = NonfatalDirectSqlError DirectSqlError Database.SQLite3.Direct.Error Utf8 | EmptyFile
  deriving (Show)

wrapFatal :: DirectSqlError -> GetSqlInfoError
wrapFatal = GetSqlInfoError "" "" . FatalError . FatalDirectSqlError

continueWith :: FilePath -> FilePath -> IO (Either (GHC.Stack.CallStack, GetSqlInfoError) SqliteStatement)
continueWith schemaDir queryPath =
  Effectful.runEff (runError applySchema)
  where
    finalizeIfNeeded = \case
      Right (Just stmt) -> do
        _ <- liftIO $ Database.SQLite3.Direct.finalize stmt
        pure ()
      _ -> pure ()

    applySchema :: Eff [Effectful.Error.Static.Error GetSqlInfoError, IOE] SqliteStatement
    applySchema = do
      sqlBs <- liftIO $ Data.ByteString.readFile queryPath
      sql <-
        (fmap encodeUtf8 . decodeUtf8') sqlBs
          & \case
            Left e -> throwError $ GetSqlInfoError queryPath sqlBs $ FatalError $ InvalidUtf8 e
            Right sql -> pure $ Utf8 sql

      bracket (liftEIO (first wrapFatal <$> Database.SQLite3.Direct.open ":memory:")) (liftEIO . fmap (first (wrapFatal . (\e -> (e, "close") :: DirectSqlError))) . Database.SQLite3.Direct.close) \db -> do
        liftEIO (first wrapFatal <$> Database.SQLite3.Direct.exec db "begin transaction")
        migrateSchema wrapFatal db schemaDir
        liftEIO (first wrapFatal <$> Database.SQLite3.Direct.exec db "commit")
        bracket (liftIO ((first \e -> (e, sql)) <$> Database.SQLite3.Direct.prepare db sql)) finalizeIfNeeded \emstmt -> do
          case emstmt of
            Right (Just stmt) -> do
              paramsCardinality <- liftIO $ bindParameterCount stmt
              paramNames <- forM [1 .. paramsCardinality] \ix -> do
                paramName <- liftIO $ Database.SQLite3.Direct.bindParameterName stmt ix
                pure (ix, paramName)
              numCols <- liftIO $ columnCount stmt
              colNames <- forM [0 .. numCols - 1] \ix -> do
                colName <- liftIO $ Database.SQLite3.Direct.columnName stmt ix
                pure (ix, colName)
              pure $ SqliteStatement {sssql = sql, ssfp = queryPath, ssParamNames = paramNames, ssResultNames = mapMaybe sequence colNames}
            Right Nothing -> throwError $ GetSqlInfoError queryPath sqlBs $ NonfatalError EmptyFile
            Left e -> do
              dse <- liftIO $ extendedErrcode db
              dsm <- liftIO $ errmsg db
              throwError $ GetSqlInfoError queryPath sqlBs $ NonfatalError $ NonfatalDirectSqlError e dse dsm

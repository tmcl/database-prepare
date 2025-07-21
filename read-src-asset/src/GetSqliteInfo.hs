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
import Data.Text.Encoding.Error
-- import Foreign
-- import Database.SQLite3.Bindings
-- import Database.SQLite3.Bindings.Types
-- import Data.Text
-- import Database.SQLite3

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
                   _ <- liftIO $ Database.SQLite3.Direct.finalize stmt
                   pure ()
                 _ -> pure ()

    applySchema ::  Eff [Effectful.Error.Static.Error GetSqlInfoError, IOE] SqliteStatement
    applySchema = do
       let path = encodeString queryPath
       sqlBs <- liftIO $ Data.ByteString.readFile path
       sql <- (fmap encodeUtf8 . decodeUtf8') sqlBs
         & \case
              Left e -> throwError $ GetSqlInfoError queryPath sqlBs $ FatalError $ InvalidUtf8 e
              Right sql -> pure $ Utf8 sql


       bracket (liftEIO (first wrapFatal <$> Database.SQLite3.Direct.open ":memory:")) (liftEIO . fmap (first (wrapFatal . (\e -> (e, "close") :: DirectSqlError))) . Database.SQLite3.Direct.close) \db -> do
           liftEIO (first wrapFatal <$> Database.SQLite3.Direct.exec db "begin transaction")
           migrateSchema wrapFatal db (Data.Set.toList schemaPaths)
           liftEIO (first wrapFatal <$> Database.SQLite3.Direct.exec db "commit")
           bracket (liftIO ((first \e -> (e, sql)) <$> Database.SQLite3.Direct.prepare db sql)) finalizeIfNeeded \emstmt -> do
               case emstmt of
                 Right (Just stmt) -> do
                   paramsCardinality <- liftIO $ bindParameterCount stmt
                   paramNames <- forM [1..paramsCardinality ] \ix -> do
                     paramName <- liftIO $ Database.SQLite3.Direct.bindParameterName stmt ix
                     pure (ix, paramName)
                   numCols <- liftIO $ columnCount stmt
                   colNames <- forM [0..numCols - 1] \ix -> do
                     colName <- liftIO $ Database.SQLite3.Direct.columnName stmt ix
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


-- * the following methods are adapted from 'Database.SQLite3.Direct' to add the tail
--
-- prepareTail' :: Database -> Utf8 -> Eff [Effectful.Error.Static.Error Database.SQLite3.Direct.Error , IOE] (Maybe Statement, Maybe Utf8)
-- prepareTail' (Database db) (Utf8 sql) =
--  withEffToIO SeqUnlift \withEff ->
--     useAsCString sql $ \sql' ->
--         alloca $ \statement ->
--           alloca $ \ztail ->
--             c_sqlite3_prepare_v2 db sql' (-1) statement ztail >>= \s -> withEff do
--                 res <- liftIO (toResultM (wrapNullablePtr Statement <$> peek statement) s)
--                   >>= \case
--                     Left err -> throwError err
--                     Right v -> pure v
--                 ctail <- liftIO $ peek ztail
--                 if ctail == nullPtr
--                   then pure (res, Nothing)
--                   else do
--                        bstail <- liftIO $ packCString ctail
--                        pure (res, Just $ Utf8 bstail)
--
--     -- Only perform the action if the 'CError' is SQLITE_OK.
-- toResultM :: Monad m => m a -> CError -> m (Either Database.SQLite3.Direct.Error a)
-- toResultM m (CError 0) = Right <$> m
-- toResultM _ code       = return $ Left $ decodeError code
--
-- wrapNullablePtr :: (Ptr a -> b) -> Ptr a -> Maybe b
-- wrapNullablePtr f ptr | ptr == nullPtr = Nothing
--                       | otherwise      = Just (f ptr)

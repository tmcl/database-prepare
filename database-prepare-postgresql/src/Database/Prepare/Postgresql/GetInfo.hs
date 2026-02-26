{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Database.Prepare.Postgresql.GetInfo (continueWith, PostgresStatement(..), ParamIndex(..), ParamInfo(..), ColumnInfo(..), prepareColumnInfo, ComparableColumnInfo(..)) where

import GHC.Stack
import Effectful
import Effectful.Error.Static
import Data.Text
import Data.Text.Encoding
import Database.PostgreSQL.LibPQ
import Data.ByteString
import Data.ByteString.Char8 qualified
import Control.Monad
import Data.Map
import Language.Haskell.TH.Syntax

newtype ParamIndex = ParamIndex Int
  deriving (Show, Eq, Ord, Num)

data ParamInfo = ParamInfo
  { paramIndex :: ParamIndex
  , paramOid :: Oid
  , paramTypeName :: Data.Text.Text
  } deriving (Show)

data PostgresStatement = PostgresStatement
  { ssfp :: FilePath
  , sssql :: ByteString
  , ssParams :: [ParamInfo]
  , ssResults :: Data.Map.Map Int ColumnInfo
  } deriving (Show)

data ComparableColumnInfo = ComparableColumnInfo {
   colIndex :: Int,
   colSqlType :: Data.Text.Text,
   colName :: Maybe ByteString
  } deriving (Show, Eq, Lift)

data ColumnInfo = ColumnInfo { colComparableInfo :: ComparableColumnInfo,
   colFormat :: Format,
   colType :: Oid,
   colMod :: Int
  } deriving (Show)

checkError :: Connection -> Maybe Result -> Eff [Effectful.Error.Static.Error GetSqlInfoError, IOE] Result
checkError conn = \case
  Just result -> do
     qstatus <- liftIO $ resultStatus result
     let buildAndThrow e = do
             msg <- liftIO $ resultErrorMessage result
             throwError (ErrorStatus e msg)
     case qstatus of
       EmptyQuery -> buildAndThrow qstatus
       BadResponse -> buildAndThrow qstatus
       NonfatalError -> buildAndThrow qstatus
       FatalError -> buildAndThrow qstatus
       PipelineSync -> buildAndThrow qstatus
       PipelineAbort -> buildAndThrow qstatus
       CommandOk -> pure result
       TuplesOk -> pure result
       SingleTuple -> pure result
       CopyOut -> pure result
       CopyIn -> pure result
       CopyBoth -> pure result
  Nothing -> do
   err <- liftIO $ maybe FailedWithoutError ErrorMessage <$> errorMessage conn
   throwError err


data GetSqlInfoError = ErrorMessage ByteString | FailedWithoutError | ErrorStatus ExecStatus (Maybe ByteString)
  deriving (Show)

lookupPgTypeName :: Connection -> Oid -> IO Data.Text.Text
lookupPgTypeName conn (Oid oid) = do
  let sql = "SELECT typname FROM pg_type WHERE oid = " <> Data.ByteString.Char8.pack (Prelude.show oid)
  exec conn sql >>= \case
    Nothing -> fail ("pg_type lookup failed for oid " <> Prelude.show oid)
    Just result -> do
      val <- getvalue result (Row 0) (Col 0)
      case val of
        Nothing -> fail ("no pg_type entry for oid " <> Prelude.show oid)
        Just bs -> pure (Data.Text.Encoding.decodeUtf8 bs)

continueWith :: (HasCallStack) => Connection -> FilePath -> IO (Either (CallStack, GetSqlInfoError) PostgresStatement)
continueWith connection queryPath = do
          Effectful.runEff (runError applySchema)

   where
    applySchema ::  Eff [Effectful.Error.Static.Error GetSqlInfoError, IOE] PostgresStatement
    applySchema = do
       sql <- liftIO $ Data.ByteString.readFile queryPath

       do
           _prepareResult <- checkError connection =<< liftIO (prepare connection "" sql Nothing)
           (prepareDescription :: Result) <- checkError connection =<< liftIO (describePrepared connection "")
           paramsCardinality <- liftIO $ nparams prepareDescription
           ssParams <- forM [0..paramsCardinality - 1] \ix -> do
                 paramOid <- liftIO $ paramtype prepareDescription ix
                 paramTypeName <- liftIO $ lookupPgTypeName connection paramOid
                 pure ParamInfo { paramIndex = ParamIndex ix, .. }

           colsCardinality <- liftIO $ nfields prepareDescription
           ssResults <- prepareColumnInfo connection colsCardinality prepareDescription

           pure PostgresStatement {sssql = sql  , ssfp = queryPath, .. }

prepareColumnInfo :: MonadIO f => Connection -> Column -> Result -> f (Map Int ColumnInfo)
prepareColumnInfo connection colsCardinality result =
           Data.Map.fromList <$> forM [0..colsCardinality - 1 ] \ix -> liftIO do
             colName <- fname result ix
             colType <- ftype result ix
             colFormat <- fformat result ix
             colMod <- fmod result ix
             let colIndex = fromIntegral ix' where Col ix' = ix
             colSqlType <- lookupPgTypeName connection colType
             let colComparableInfo = ComparableColumnInfo{..}
             pure (colIndex, ColumnInfo {..})

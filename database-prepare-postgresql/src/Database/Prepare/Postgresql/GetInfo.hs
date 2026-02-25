{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Database.Prepare.Postgresql.GetInfo (continueWith, PostgresStatement(..), ParamIndex(..), ColumnInfo(..), prepareColumnInfo, ComparableColumnInfo(..)) where

import Data.Function
import GHC.Stack
import Effectful
import Effectful.Error.Static
import Data.Text
import Database.PostgreSQL.LibPQ
import Data.ByteString
import Control.Monad
import Debug.Trace
import Data.Map
import Language.Haskell.TH.Syntax
import Database.Prepare.Postgresql.FromField (builtinOids)

newtype ParamIndex = ParamIndex Int
  deriving (Show, Eq, Num)

data PostgresStatement = PostgresStatement
  { ssfp :: FilePath
  , sssql :: ByteString
  , ssParams :: [(ParamIndex, Oid)]
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
                 paramType <- liftIO $ paramtype prepareDescription ix
                 let ans = (ParamIndex ix, paramType)
                 Debug.Trace.traceShowM ans
                 pure ans

           colsCardinality <- liftIO $ nfields prepareDescription
           ssResults <- prepareColumnInfo colsCardinality prepareDescription

           pure PostgresStatement {sssql = sql  , ssfp = queryPath, .. }

prepareColumnInfo :: MonadIO f => Column -> Result -> f (Map Int ColumnInfo)
prepareColumnInfo colsCardinality result =
           Data.Map.fromList <$> forM [0..colsCardinality - 1 ] \ix -> liftIO do
             colName <- fname result ix
             colType <- ftype result ix
             colFormat <- fformat result ix
             colMod <- fmod result ix
             let colIndex = fromIntegral ix' where Col ix' = ix
             let Oid cOid  = colType
             colSqlType <- builtinOids (fromIntegral cOid)
               & \case
                 Nothing -> fail (Prelude.unwords ["could not identify sqltype for", Prelude.show colType])
                 Just it -> pure $ Data.Text.pack it
             let colComparableInfo = ComparableColumnInfo{..}
             let ans = ColumnInfo {..}
             Debug.Trace.traceShowM ans
             pure (colIndex, ans)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.String.Interpolate
import Effectful.Exception
import Data.Foldable
import Data.Bifunctor
import Effectful
import Effectful.Error.Static
import MigrateSchema
import Database.SQLite3.Direct
import System.IO
import System.Exit
import Data.Text.Encoding
import Data.ByteString
import Control.Monad
import System.FSNotify
import Data.Aeson
import Data.ByteString.Lazy
import System.Environment
import Data.Text
import Data.Maybe
import System.Directory
import Filesystem
import Filesystem.Path.CurrentOS
import Data.List
import Debug.Trace
import Data.Set

data Arguments = Arguments
  { out :: Maybe Filesystem.Path.CurrentOS.FilePath
  , paths :: Data.Set.Set Filesystem.Path.CurrentOS.FilePath
  , argSchemaPaths :: Data.Set.Set Filesystem.Path.CurrentOS.FilePath
  } deriving (Show)

main :: IO ()
main = worker

worker :: HasCallStack => IO ()
worker = do
  cwd <- getCurrentDirectory
  args <- System.Environment.getArgs
  Data.ByteString.Lazy.putStr (Data.Aeson.encode args <> "\n")
  let arguments = Data.List.foldr (\arg accum -> case arg of
                                   theArg | Just outPath <- Data.List.stripPrefix "--out=" theArg -> accum { out = Just (Filesystem.Path.CurrentOS.decodeString outPath) }
                                              | Just schemaPath <- Data.List.stripPrefix "--schema=" theArg -> accum {
                                                  argSchemaPaths = Filesystem.Path.CurrentOS.decodeString schemaPath `Data.Set.insert` accum.argSchemaPaths,
                                                  paths = Filesystem.Path.CurrentOS.decodeString schemaPath `Data.Set.insert` accum.paths }
                                   _ -> accum { paths = Filesystem.Path.CurrentOS.decodeString arg `Data.Set.insert` accum.paths }
                         ) (Arguments Nothing Data.Set.empty Data.Set.empty ) args

  let prefix = fromMaybe "" arguments.out
  isDirectories <- forM (Data.Set.toList arguments.paths) \path -> do
    isdir <- Filesystem.isDirectory path
    pure (path, isdir)
  let (others, directories) = Data.List.foldr (\(c, b) (accum1, accum2) -> if b then (accum1, c `Data.Set.insert` accum2) else (c `Data.Set.insert` accum1, accum2)) (Data.Set.empty, Data.Set.empty) isDirectories

  schemaPaths <- Data.Set.fromList . join <$> forM (Data.Set.toList arguments.argSchemaPaths) \dir -> do
       dirContent <- Filesystem.listDirectory dir
       forM dirContent (\fi -> Filesystem.isFile fi  >>= guard >> pure fi)

  forM_ directories \dir -> do
       dirContent <- Filesystem.listDirectory dir
       let dirContentSql = Prelude.filter (Data.List.isSuffixOf ".sql" . encodeString) dirContent
       forM_ dirContentSql \sql -> do
          let txt = dir  </> filename sql
          continueWith schemaPaths prefix txt

  when (not $ Prelude.null others) do
    fail "will not continue with non-directories"
  when (Prelude.null directories) do
    fail "no directories with which to continue"
  let interestingEvent = \case
         Added {} -> True
         Modified {} -> True
         _ -> False
  withManager \inotify -> do
    forM_ directories \dir -> do
       watchDir inotify  (encodeString dir) interestingEvent \event -> do
         let rawfp = eventPath event
         case Data.List.stripPrefix (cwd <> "/") rawfp of
          Nothing -> pure ()
          Just fn -> do
             let txt1 = Data.Text.pack fn

             let txt = fromMaybe txt1 (Data.Text.stripSuffix ".json" txt1)
             when (Data.Text.isSuffixOf ".sql" txt) do
               continueWith schemaPaths prefix (fromText txt)

    print =<< getChar

data SqliteStatement = SqliteStatement
  { ssfp :: Filesystem.Path.CurrentOS.FilePath
  , ssParamNames :: [(ParamIndex, Maybe Utf8)]
  , ssResultNames :: [(ColumnIndex, Utf8)]
  } deriving (Show)

toTypescript :: SqliteStatement -> Data.ByteString.Lazy.ByteString
toTypescript stmt = [__i|
    import * as SQLite from 'expo-sqlite';

    export type Params = {#{params}}
    export type Result = {#{resultTy}}

    export function getFirstAsync (db: SQLite.SQLiteDatabase) {
        return async function (params: Params) {
            const stmt = await db.prepareAsync(require(#{tsFilename}));
            try {
                const result = await stmt.executeAsync<Result>(params);
                return await result.getFirstAsync();
            } finally {
                await stmt.finalizeAsync();
            }
        }
    }

    export function getAllAsync (db: SQLite.SQLiteDatabase) {
        return async function (params: Params) {
            const stmt = await db.prepareAsync(require(#{tsFilename}));
            try {
                const result = await stmt.executeAsync<Result>(params);
                return await result.getAllAsync();
            } finally {
                await stmt.finalizeAsync();
            }
        }
    }
 |]
  where
    tsFilename = Data.Aeson.encode $ "./" <> encodeString (filename (ssfp stmt) <.> "json")
    resultTy = Data.Text.intercalate ", " (Data.List.map (\(_ix, name) -> [__i|#{name}: string|number|null|]) (ssResultNames stmt))
    tsParamName = \cases
      _ (Just it) -> it
      ix Nothing -> [__i|#{ix}|]

    params = Data.Text.intercalate ", " (Data.List.map (\(ix, name) -> [__i|#{tsParamName ix name}: string|number|null|]) (ssParamNames stmt))

continueWith :: (HasCallStack) => Data.Set.Set Filesystem.Path.CurrentOS.FilePath -> Filesystem.Path.CurrentOS.FilePath -> Filesystem.Path.CurrentOS.FilePath -> IO ()
continueWith schemaPaths outPrefix  queryPath = do
        if Data.Set.member queryPath schemaPaths
          then basicEncode
          else Effectful.runEff (runError applySchema >>= \case
              Left e -> liftIO $ hPrint stderr (e :: (CallStack, DirectSqlError)) >> exitFailure
              Right () -> pure ())

   where
    finalizeIfNeeded = \case
                 Right (Just stmt) -> do
                   void $ liftIO $ finalize stmt
                 _ -> pure ()
    applySchema = do
       let path = encodeString queryPath
       sql <- liftIO $ Utf8 <$> Data.ByteString.readFile path
       mstmtInfo <- bracket (liftEIO $ open ":memory:") (liftEIO . fmap (first \e -> (e, "close") :: DirectSqlError) . close) \db -> do
           liftEIO $ exec db "begin transaction"
           migrateSchema db (Data.Set.toList schemaPaths)
           liftEIO $ exec db "commit"
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
                   pure $ Just SqliteStatement { ssfp = queryPath, ssParamNames = paramNames, ssResultNames = mapMaybe sequence colNames }
                 _ -> pure Nothing -- in case of a syntax error, we don't care, just let them keep editing
       forM_ mstmtInfo \stmtInfo -> do
         liftIO $ writeIfDifferent (outPrefix </> queryPath <.> "ts") (toTypescript stmtInfo)
         liftIO $ writeIfDifferent (outPrefix </> queryPath <.> "json") (Data.Aeson.encode $ utf8Text sql)
         pure ()
    writeIfDifferent path newData = do
        needsUpdate <- doesNeedUpdate path newData
        when needsUpdate do
          createDirectoryIfMissing True (encodeString $ directory path)
          Data.ByteString.Lazy.writeFile (encodeString path) newData
    doesNeedUpdate path newData = do
          f <- isFile path
          if f
            then do
              oldData <- Data.ByteString.Lazy.readFile (encodeString path)
              let x = oldData /= newData
              when x do
                  Debug.Trace.traceShowM ("needs update because"::String, show oldData, "is different from" :: String, show newData)
              pure x
            else Debug.Trace.traceShowM ("needs update because"::String, show path, "does not exist" :: String) >> pure True
    basicEncode = do
        let str = Filesystem.Path.CurrentOS.encodeString queryPath
        sql <- Data.ByteString.readFile str >>= (\case
            Left e -> hPrint stderr (e, str) >> exitFailure
            Right r -> pure r) . decodeUtf8'
        let strSqlJson = outPrefix </> queryPath <.> "json"
        let newSqlJson = Data.Aeson.encode sql
        writeIfDifferent strSqlJson newSqlJson

utf8Text :: Utf8 -> Text
utf8Text (Utf8 bs) = decodeUtf8 bs
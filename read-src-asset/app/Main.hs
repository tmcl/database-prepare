{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import GetSqliteInfo

import Data.String.Interpolate
import Data.Foldable
import Data.Function
import Effectful
import Effectful.Error.Static
import Database.SQLite3.Direct
import System.IO
import System.Exit
import Data.Text.Encoding
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
  allArgs <- System.Environment.getArgs
  let (watches, args) = maybe (False, allArgs) (\restArgs -> (True, restArgs)) $ Data.List.stripPrefix ["--watch"] allArgs
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
       forM dirContent (\fi -> fi <$ (Filesystem.isFile fi  >>= guard))

  forM_ directories \dir -> do
       dirContent <- Filesystem.listDirectory dir
       let dirContentSql = Prelude.filter (Data.List.isSuffixOf ".sql" . encodeString) dirContent
       forM_ dirContentSql \sql -> do
          let txt = dir  </> filename sql
          Main.continueWith schemaPaths prefix txt

  when (not $ Prelude.null others) do
    fail "will not continue with non-directories"
  when (Prelude.null directories) do
    fail "no directories with which to continue"
  let interestingEvent = \case
         Added {} -> True
         Modified {} -> True
         _ -> False
  when watches do
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
                   Main.continueWith schemaPaths prefix (fromText txt)

        print =<< getChar

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
        GetSqliteInfo.continueWith schemaPaths queryPath
         >>= \case
          Left e -> hPrint stderr e >> exitFailure
          Right thingames -> case thingames of
              JustSql sql -> do
                let strSqlJson = outPrefix </> queryPath <.> "json"
                txtSql <- decodeUtf8' sql & \case
                    Left e -> hPrint stderr (e, sql) >> exitFailure -- this ought never to happen because we already returned a fatal error
                    Right r -> pure r
                let newSqlJson = Data.Aeson.encode $ txtSql
                writeIfDifferent strSqlJson newSqlJson
              stmtInfo@SqliteStatement { } -> do
                 liftIO $ writeIfDifferent (outPrefix </> stmtInfo.ssfp <.> "ts") (toTypescript stmtInfo)
                 liftIO $ writeIfDifferent (outPrefix </> stmtInfo.ssfp <.> "json") (Data.Aeson.encode $ utf8Text stmtInfo.sssql)

   where
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

utf8Text :: Utf8 -> Text
utf8Text (Utf8 bs) = decodeUtf8 bs
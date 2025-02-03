{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import System.FSNotify
import Data.Aeson
import Data.ByteString.Lazy
import System.Environment
import Data.Text
import Data.Maybe
import Data.Function
import System.Directory
import Filesystem
import Filesystem.Path.CurrentOS hiding (directory)
import Data.Either
import Data.List
import Data.Text.IO hiding (putStrLn)
import Debug.Trace

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  args <- System.Environment.getArgs
  print args
  let (option, paths) = partitionEithers $ ((\arg -> maybe (Right arg) Left $ Data.List.stripPrefix "--out=" arg) <$> args)
  let prefix = maybe "" (<> "/") (listToMaybe option)
  isDirectories <- forM paths \path -> do
    let path1 = decodeString path
    isdir <- Filesystem.isDirectory path1
    let path3 = toText path1 & \case
          Right r -> r
          Left e -> error (show e)
    let path2 = Data.Text.unpack path3
    pure (path1, path2 :: String, isdir)
  let (others, directories) = partitionEithers $ fmap (\(c, a, b) -> if b then pure (c, a) else Left a) isDirectories
  print directories
  forM_ directories \(directory, directory2) -> do
       dirContent <- Filesystem.listDirectory directory
       let dirContentSql = Prelude.filter (Data.List.isSuffixOf ".sql" . encodeString) dirContent
       print dirContentSql
       forM_ dirContentSql \sql -> do
          let txt = Data.Text.pack directory2  <> "/" <> either (error . show) id (toText $ filename sql)
          continueWith prefix txt

  unless (Prelude.null others) do
    print others
    fail "will not continue with non-directories"
  when (Prelude.null directories) do
    print others
    fail "no directories with which to continue"
  let interestingEvent = \case
         Added {} -> True
         Modified {} -> True
         _ -> False
  putStrLn "want to watch"
  print directories
  withManager \inotify -> do
    forM_ directories \(_, directory) -> do
       watchDir inotify  directory interestingEvent \event -> do
         print event
         putStrLn "hi1"
         let rawfp = eventPath event
         case Data.List.stripPrefix (cwd <> "/") rawfp of
          Nothing -> pure ()
          Just fp -> do
             let txt1 = Data.Text.pack fp
             putStrLn "hi2"

             print txt1
             putStrLn "hi3"
             let txt = fromMaybe txt1 (Data.Text.stripSuffix ".json" txt1)
             putStrLn "hi4"
             print (txt, txt1)
             putStrLn "hi5"
             when (Data.Text.isSuffixOf ".sql" txt) do
               putStrLn "hi6"
               print txt
               continueWith prefix txt

    print =<< getChar


continueWith :: Prelude.FilePath -> Text -> IO ()
continueWith outPrefix  txt = do
        let str = Data.Text.unpack txt
        let path1 = fromText txt

        print str
        sql <- Data.Text.IO.readFile str
        let strSqlJson = outPrefix <> str <> ".json"
        let sqljsonFile = fromText $ Data.Text.pack strSqlJson
        print (path1, sqljsonFile)
        let newSqlJson = Data.Aeson.encode sql
        needsUpdate <- do
          f <- isFile sqljsonFile
          print f
          if f
            then do
              oldsqljson <- Data.ByteString.Lazy.readFile strSqlJson
              let x = oldsqljson /= newSqlJson
              when x do
                  Debug.Trace.traceShowM ("needs update because"::String, show oldsqljson, "is different from" :: String, show newSqlJson)
              pure x
            else Debug.Trace.traceShowM ("needs update because"::String, show sqljsonFile, "does not exist" :: String) >> pure True
        when needsUpdate do
          Data.ByteString.Lazy.writeFile strSqlJson newSqlJson

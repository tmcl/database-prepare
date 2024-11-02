{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import System.INotify
import Data.Aeson
import Data.ByteString.Lazy
import System.Environment
import Data.Text
import Data.Maybe
import Data.Function
import Filesystem
import Filesystem.Path.CurrentOS hiding (directory)
import Data.Either
import Data.Text.Encoding
import Data.List
import Data.Text.IO hiding (putStrLn)

main :: IO ()
main = do
  let continueWith dir baretxt = do
        let txt = Data.Text.Encoding.decodeUtf8 dir <> "/" <> baretxt
        let path1 = fromText txt
        print (path1)
        
        let str = Data.Text.unpack txt
        sql <- Data.Text.IO.readFile str
        let sqljsonFile = path1 <> ".json"
        let newSqlJson = Data.Aeson.encode sql
        needsUpdate <- do
          f <- isFile sqljsonFile
          if f 
            then do
              oldsqljson <- Data.ByteString.Lazy.readFile (str <> ".json")
              pure $ oldsqljson /= newSqlJson
            else pure True
        when needsUpdate do
          Data.ByteString.Lazy.writeFile (str <> ".json") newSqlJson
         
  
  paths <- System.Environment.getArgs
  print paths
  isDirectories <- forM paths \path -> do
    let path1 = decodeString path
    isdir <- Filesystem.isDirectory path1
    let path3 = (toText path1) & \case 
          Right r -> r
          Left e -> error (show e)
    let path2 = encodeUtf8 path3
    pure (path1, path2, isdir)
  let (others, directories) = partitionEithers $ fmap (\(c, a, b) -> if b then pure (c, a) else Left a) isDirectories
  print directories
  forM_ directories \(directory, directory2) -> do
       dirContent <- listDirectory directory
       let dirContentSql = Prelude.filter (Data.List.isSuffixOf ".sql" . encodeString) dirContent
       print dirContentSql
       forM_ dirContentSql \sql -> do
          continueWith directory2 (either (error . show) id $ toText $ filename sql)
 
  unless (Prelude.null others) do
    print others
    fail "will not continue with non-directories"
  withINotify \inotify -> do
    forM_ directories \(_, directory) -> do
       addWatch inotify [Modify, CloseWrite, Move, MoveIn, MoveOut, Create, Delete] directory \event -> do
         print event
         putStrLn "hi1" 
         let fp = fromJust (maybeFilePath event)
         print $ Data.Text.Encoding.decodeUtf8Lenient fp
         let txt1 = Data.Text.Encoding.decodeUtf8 fp
         putStrLn "hi2" 

         print (txt1)
         putStrLn "hi3" 
         let txt = fromMaybe txt1 (Data.Text.stripSuffix ".json" txt1)
         putStrLn "hi4" 
         print (txt, txt1)
         putStrLn "hi5" 
         when (Data.Text.isSuffixOf ".sql" txt) do
           putStrLn "hi6" 
           print txt
           continueWith directory txt
          
        
    print =<< getChar
  

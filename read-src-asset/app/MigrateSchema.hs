{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module MigrateSchema where

import Database.SQLite3.Direct
import Control.Monad
import Data.ByteString
import Effectful
import Effectful.Error.Static
import Filesystem.Path.CurrentOS hiding (directory)

type DirectSqlError = (Database.SQLite3.Direct.Error, Utf8)

liftEIO :: (Show a, Effectful.Error.Static.Error a :> es, IOE :> es) => IO (Either a b) -> Eff es b
liftEIO mab = liftIO mab >>= \case
  Left a -> throwError a
  Right b -> pure b

migrateSchema :: (Effectful.Error.Static.Error DirectSqlError :> es, IOE :> es) => Database -> [Filesystem.Path.CurrentOS.FilePath] -> Eff es ()
migrateSchema conn files = do
  forM_ files \file -> do

    sql <- liftIO $ Utf8 <$> Data.ByteString.readFile (encodeString file)
    liftEIO $ exec conn sql
  pure ()
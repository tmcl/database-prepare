{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -ddump-splices #-}

module BasicMapped (searchUsers) where

import Data.ByteString
import Database.PostgreSQL.LibPQ
import Database.Postgres.Temp
import Database.Prepare.Postgresql.TH
import Language.Haskell.TH
import MappedTypes 

$(do
    Right pgdb <- runIO $ startConfig defaultConfig
    conn <- runIO $ connectdb (toConnectionString pgdb)
    schemaSql <- runIO $ Data.ByteString.readFile "test-data/schema/schema.sql"
    schema <- runIO $ exec conn schemaSql

    case schema of
      Nothing -> fail "could not use db connection"
      Just f -> do
         runIO (resultErrorMessage f)
           >>= \case
             Nothing -> pure ()
             Just "" -> pure ()
             Just err -> fail . Prelude.show $ err
    f <- embedPostgresMapped conn "test-data/query/basic-named.sql" "searchUsers"
      (Just (''UserSearch, fieldPairs userSearchParamMap))
      (Just (''User, fieldPairs userResultMap))
    runIO $ finish conn
    runIO $ stop pgdb
    pure f
  )

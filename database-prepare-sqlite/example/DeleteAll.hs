{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -ddump-splices #-}

module DeleteAll where

import Database.Prepare.Sqlite.TH

$(embedSqlite "test-data/schema" "test-data/query/delete-all.sql" "deleteAll")

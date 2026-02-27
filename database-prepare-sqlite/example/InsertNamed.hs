{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module InsertNamed where

import Data.Row.Records
import Database.Prepare.Sqlite.TH

$(embedSqlite "test-data/schema" "test-data/query/insert-named.sql" "insertNamed")

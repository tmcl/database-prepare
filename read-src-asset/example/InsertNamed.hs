{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module InsertNamed where

import SqliteTH
import Data.Row.Records

$( embedSqlite ["test-data/schema/schema.sql"] "test-data/query/insert-named.sql" "insertNamed" )
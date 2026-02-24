{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module InsertNamed where

import Data.Row.Records
import SqliteTH

$(embedSqlite ["test-data/schema/schema.sql"] "test-data/query/insert-named.sql" "insertNamed")

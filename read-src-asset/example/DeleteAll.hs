{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -ddump-splices #-}

module DeleteAll where

import SqliteTH

$(embedSqlite ["test-data/schema/schema.sql"] "test-data/query/delete-all.sql" "deleteAll")

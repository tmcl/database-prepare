{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -ddump-splices #-}

module DeleteNamed where

import Database.Prepare.Sqlite.TH

$(embedSqlite ["test-data/schema/schema.sql"] "test-data/query/delete-named.sql" "deleteUser")

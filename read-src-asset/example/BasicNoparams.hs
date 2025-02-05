{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -ddump-splices #-}


module BasicNoparams where

import SqliteTH
import Data.Row.Records

$(embedSqlite ["test-data/schema/schema.sql"] "test-data/query/basic-noparams.sql" "queryAll" )



{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Database.Prepare.Postgresql.PgType where

import Data.Int
import Data.Kind
import Data.Text
import Data.Time
import GHC.TypeLits

type family PgType (pgTypeName :: Symbol) :: Type

type instance PgType "int4" = Int32
type instance PgType "text" = Text
type instance PgType "timestamptz" = UTCTime

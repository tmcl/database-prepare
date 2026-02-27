{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Database.Prepare.Sqlite.TH (
  -- * Original API
  embedSqlite,
  -- * Mapped API
  embedSqliteMapped, deriveMappingType,
  GFieldPairs(..), fieldPairs,
) where

import Data.Char
import Control.Monad
import Data.Function
import Data.List
import Data.Map qualified
import Data.Row.Records
import Data.Set
import Data.Text
import Data.Text.Encoding
import Data.Traversable
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow (fieldWith)
import Database.SQLite.Simple.Internal
import Database.SQLite3.Direct qualified
import Database.Prepare.Sqlite.GetInfo
import GHC.Generics qualified
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Quasi (qAddDependentFile))

strUtf8 :: Database.SQLite3.Direct.Utf8 -> String
strUtf8 (Database.SQLite3.Direct.Utf8 a) = Data.Text.unpack $ decodeUtf8 a

txtUtf8 :: Database.SQLite3.Direct.Utf8 -> Text
txtUtf8 (Database.SQLite3.Direct.Utf8 a) = decodeUtf8 a

embedSqlite :: Data.Set.Set FilePath -> FilePath -> String -> Q [Dec]
embedSqlite schemas fpQuery name = do
  qAddDependentFile fpQuery
  mapM_ qAddDependentFile (toList schemas)
  let query1 = mkName name
  let capName = case name of
        n : ame -> Data.Char.toUpper n : ame
        _ -> name
  sqlite <- runIO do continueWith schemas fpQuery
  case sqlite of
    Left (cs, err) -> fail (Prelude.show cs <> Prelude.show err)
    Right (JustSql bs) -> do
      let connName = mkName "conn"
      let conn = pure $ VarE $ connName
      txtSql <-
        decodeUtf8' bs & \case
          Left e -> fail (Prelude.show e)
          Right txt -> pure txt
      let c = clause @Q [pure $ VarP connName] (normalB [|Database.SQLite.Simple.execute_ $(conn) (Query txtSql)|]) []
      Data.Traversable.sequence [sigD (query1) [t|Connection -> IO ()|], funD query1 [c]]
    Right stmt@(SqliteStatement _ (Database.SQLite3.Direct.Utf8 bs) params results) -> do
      let connName = mkName "conn"
      let conn = pure $ VarE $ connName
      txtSql <-
        decodeUtf8' bs & \case
          Left e -> fail (Prelude.show e)
          Right txt -> pure txt
      runIO $ print stmt
      let paramsTyNam = mkName (capName <> "Params")
      let resultsTyNam = mkName (capName <> "Result")
      let paramsVarName = mkName "params"

      let paramsDecs = case params of
            [] -> []
            _ -> [rowType, toNamedParamsTy, toNamedParamsDef]
              where
                paramToNamedParam :: Database.SQLite3.Direct.ParamIndex -> Maybe Database.SQLite3.Direct.Utf8 -> Q Exp
                paramToNamedParam ix = \case
                  Just (Database.SQLite3.Direct.Utf8 v) -> let fieldName = Data.Text.unpack (decodeUtf8 v) in defineConversion fieldName
                  Nothing -> let fieldName = "param" <> Prelude.show ix in defineConversion fieldName
                  where
                    defineConversion fieldName = [|$(stringE fieldName) := ($(varE paramsVarName) .! $(appTypeE (conE 'Label) (pure $ LitT $ StrTyLit fieldName)))|]
                toNamedParamsDef :: Q Dec
                toNamedParamsDef = funD toNamedParamsVarName [clause @Q [pure $ ConP paramsTyNam [] [VarP paramsVarName]] (normalB $ listE $ (uncurry paramToNamedParam) <$> params) []]
                toNamedParamsVarName = mkName ("toNamed" <> capName <> "Params")
                toNamedParamsTy :: Q Dec
                toNamedParamsTy = sigD toNamedParamsVarName [t|$(pure $ ConT paramsTyNam) -> [NamedParam]|]
                rowType = newtypeD @Q (pure []) paramsTyNam [] Nothing (normalC paramsTyNam [bangTy]) [derivClause Nothing [[t|Show|], [t|Eq|]]]
                  where
                    bangTy = do
                      app <- appT [t|Rec|] recTyArgs
                      pure (Bang NoSourceUnpackedness NoSourceStrictness, app)
                    buildArg :: (Database.SQLite3.Direct.ParamIndex, Maybe Database.SQLite3.Direct.Utf8) -> Q Type
                    buildArg (ix, mname) = [t|$(pure $ LitT $ StrTyLit $ maybe (Prelude.show ix) (Data.Text.unpack . txtUtf8) mname) .== SQLData|]
                    recTyArgs = Data.List.foldr1 (\ty1 ty2 -> infixT ty1 (mkName "Data.Row.Records..+") ty2) (buildArg <$> params)

      let resultsDecs = case results of
            [] -> []
            _ -> [resultsTy, resultsInstance]
              where
                resultsTy = newtypeD @Q (pure []) resultsTyNam [] Nothing (normalC resultsTyNam [bangTy]) [derivClause Nothing []]
                  where
                    bangTy = do
                      app <- appT [t|Rec|] recTyArgs
                      pure (Bang NoSourceUnpackedness NoSourceStrictness, app)
                    buildArg :: (Database.SQLite3.Direct.ColumnIndex, Database.SQLite3.Direct.Utf8) -> Q Type
                    buildArg (_ix, mname) = [t|$(pure $ LitT $ StrTyLit $ (Data.Text.unpack . txtUtf8) mname) .== Field|]
                    recTyArgs = Data.List.foldr1 (\ty1 ty2 -> infixT ty1 (mkName "Data.Row.Records..+") ty2) (buildArg <$> results)
                resultsInstance =
                  instanceD @Q
                    (pure [])
                    [t|FromRow $(pure $ ConT resultsTyNam)|]
                    [ funD
                        'fromRow
                        [ clause @Q
                            []
                            ( normalB $ doE $
                                bindings ++ [noBindS [|pure $ $(conE resultsTyNam) $(builtRecord)|]]
                            )
                            []
                        ]
                    ]
                  where
                    fieldNames = [mkName ("f" <> Prelude.show i) | i <- [0 .. Data.List.length results - 1]]
                    bindings = [bindS (varP fn) [|fieldWith pure|] | fn <- fieldNames]
                    builtRecord = Data.List.foldr recordBuilder [|Data.Row.Records.empty|] (Data.List.zip fieldNames results)
                    recordBuilder (fn, (_ix, utf8)) rest =
                      let nm = strUtf8 utf8
                      in [|$(appTypeE (conE 'Label) (pure $ LitT $ StrTyLit nm)) .== $(varE fn) .+ $rest|]

      let toNamedParamsVarName = mkName ("toNamed" <> capName <> "Params")
      let implementation = case (params, results) of
            ([], []) -> [sigD query1 [t|Connection -> IO ()|], funD query1 [c]]
              where
                c = clause @Q [pure $ VarP connName] (normalB [|Database.SQLite.Simple.execute_ $(conn) (Query txtSql)|]) []
            ([], _) -> [sigD query1 [t|Connection -> IO [$(pure $ ConT resultsTyNam)]|], funD query1 [c]]
              where
                c = clause @Q [pure $ VarP connName] (normalB [|Database.SQLite.Simple.query_ $(conn) (Query txtSql)|]) []
            (_, []) -> [sigD query1 [t|Connection -> $(pure $ ConT paramsTyNam) -> IO ()|], funD query1 [c]]
              where
                c = clause @Q [pure $ VarP connName, pure $ VarP paramsVarName] (normalB [|Database.SQLite.Simple.executeNamed $(conn) (Query txtSql) ($(varE toNamedParamsVarName) $(varE paramsVarName))|]) []
            (_, _) -> [sigD query1 [t|Connection -> $(pure $ ConT paramsTyNam) -> IO [$(pure $ ConT resultsTyNam)]|], funD query1 [c]]
              where
                c = clause @Q [pure $ VarP connName, pure $ VarP paramsVarName] (normalB [|Database.SQLite.Simple.queryNamed $(conn) (Query txtSql) ($(varE toNamedParamsVarName) $(varE paramsVarName))|]) []

      Data.Traversable.sequence (paramsDecs <> resultsDecs <> implementation)


-- ---------------------------------------------------------------------------
-- Mapped API: deriveMappingType, GFieldPairs, fieldPairs, embedSqliteMapped
-- ---------------------------------------------------------------------------

-- | Derive a companion mapping type with the same field names but all 'String' fields.
-- For a type @User@ with fields @uId@, @uName@, this generates:
--
-- > data UserFieldNames = UserFieldNames { uId :: String, uName :: String, ... }
-- >   deriving (GHC.Generics.Generic, Show)
deriveMappingType :: Name -> Q [Dec]
deriveMappingType typeName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ [RecC conName fields] _) -> mkMappingType conName fields
    TyConI (NewtypeD _ _ _ _ (RecC conName fields) _) -> mkMappingType conName fields
    _ -> fail $ "deriveMappingType: " <> nameBase typeName <> " must be a single-constructor record type"
  where
    mappingTypeName = mkName (nameBase typeName <> "FieldNames")
    mappingConName = mkName (nameBase typeName <> "FieldNames")
    mkMappingType _ fields = do
      let mappingFields = (\(fieldName, _, _) -> (fieldName, noBang, ConT ''String)) <$> fields
          noBang = Bang NoSourceUnpackedness NoSourceStrictness
      pure [ DataD [] mappingTypeName [] Nothing
               [RecC mappingConName mappingFields]
               [DerivClause Nothing [ConT ''GHC.Generics.Generic, ConT ''Show]]
           ]

-- | Typeclass for extracting @[(fieldName, value)]@ from a Generic representation.
class GFieldPairs f where
  gFieldPairs :: f p -> [(String, String)]

instance GFieldPairs GHC.Generics.U1 where
  gFieldPairs GHC.Generics.U1 = []

instance GFieldPairs f => GFieldPairs (GHC.Generics.D1 m f) where
  gFieldPairs (GHC.Generics.M1 x) = gFieldPairs x

instance GFieldPairs f => GFieldPairs (GHC.Generics.C1 m f) where
  gFieldPairs (GHC.Generics.M1 x) = gFieldPairs x

instance (GHC.Generics.Selector m) => GFieldPairs (GHC.Generics.S1 m (GHC.Generics.Rec0 String)) where
  gFieldPairs sel@(GHC.Generics.M1 (GHC.Generics.K1 val)) = [(GHC.Generics.selName sel, val)]

instance (GFieldPairs l, GFieldPairs r) => GFieldPairs (l GHC.Generics.:*: r) where
  gFieldPairs (l GHC.Generics.:*: r) = gFieldPairs l <> gFieldPairs r

-- | Extract @[(fieldName, value)]@ from a record whose fields are all 'String'.
fieldPairs :: (GHC.Generics.Generic a, GFieldPairs (GHC.Generics.Rep a)) => a -> [(String, String)]
fieldPairs = gFieldPairs . GHC.Generics.from


-- ---------------------------------------------------------------------------
-- Verification
-- ---------------------------------------------------------------------------

-- | Extract record fields and data constructor name from a reified type.
extractRecFields :: Name -> Q (Name, [(Name, Language.Haskell.TH.Type)])
extractRecFields typeName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ [RecC conName fields] _) -> pure (conName, [(n, t) | (n, _, t) <- fields])
    TyConI (NewtypeD _ _ _ _ (RecC conName fields) _) -> pure (conName, [(n, t) | (n, _, t) <- fields])
    _ -> fail $ nameBase typeName <> " must be a single-constructor record type"

-- | Verify a result mapping against the query's result columns.
-- Returns (conName, fields ordered by column index) for codegen.
verifySqliteResultMapping
  :: Name
  -> [(String, String)]
  -> [(Database.SQLite3.Direct.ColumnIndex, Database.SQLite3.Direct.Utf8)]
  -> Q (Name, [(Name, Database.SQLite3.Direct.ColumnIndex)])
verifySqliteResultMapping typeName mapping resultCols = do
  (conName, fields) <- extractRecFields typeName
  let mappingMap = Data.Map.fromList mapping
      fieldNameSet = Data.Map.fromList [(nameBase n, n) | (n, _) <- fields]
      colsByName = Data.Map.fromList
        [(Data.Text.unpack (txtUtf8 nm), ix) | (ix, nm) <- resultCols]

  -- Check every field has a mapping
  forM_ fields \(fieldName, _) ->
    case Data.Map.lookup (nameBase fieldName) mappingMap of
      Nothing -> fail $ "Field " <> nameBase fieldName <> " of " <> nameBase typeName <> " has no mapping"
      Just _ -> pure ()

  -- Check every mapping key corresponds to a field
  forM_ (Data.Map.keys mappingMap) \key ->
    case Data.Map.lookup key fieldNameSet of
      Nothing -> fail $ "Mapping key " <> key <> " does not correspond to a field in " <> nameBase typeName
      Just _ -> pure ()

  -- For each mapping, resolve column
  verified <- forM (Data.Map.toList mappingMap) \(fieldNameStr, sqlColName) ->
    case (Data.Map.lookup fieldNameStr fieldNameSet, Data.Map.lookup sqlColName colsByName) of
      (Nothing, _) -> fail $ "Field " <> fieldNameStr <> " not found in " <> nameBase typeName
      (_, Nothing) -> fail $ "SQL column " <> sqlColName <> " not found in query result"
      (Just fieldName, Just colIdx) -> pure (fieldName, colIdx)

  -- Check all columns are covered
  let mappedColNames = Data.Map.fromList [(snd pair, ()) | pair <- mapping]
  forM_ resultCols \(_ix, nm) ->
    let colName = Data.Text.unpack (txtUtf8 nm)
    in unless (Data.Map.member colName mappedColNames) $
         fail $ "SQL column " <> colName <> " is not covered by any field mapping"

  -- Sort by column index
  pure (conName, Data.List.sortOn snd verified)

-- | Verify a param mapping against the query's named parameters.
-- Returns fields ordered by param index for codegen.
verifySqliteParamMapping
  :: Name
  -> [(String, String)]
  -> [(Database.SQLite3.Direct.ParamIndex, Maybe Database.SQLite3.Direct.Utf8)]
  -> Q [(Name, String, Database.SQLite3.Direct.ParamIndex)]
verifySqliteParamMapping typeName mapping paramNames = do
  (_conName, fields) <- extractRecFields typeName
  let mappingMap = Data.Map.fromList mapping
      fieldNameSet = Data.Map.fromList [(nameBase n, n) | (n, _) <- fields]
      -- Build lookup from param name string to param index
      paramsByName = Data.Map.fromList
        [(Data.Text.unpack (txtUtf8 nm), ix)
        | (ix, Just nm) <- paramNames]

  -- Check every field has a mapping
  forM_ fields \(fieldName, _) ->
    case Data.Map.lookup (nameBase fieldName) mappingMap of
      Nothing -> fail $ "Field " <> nameBase fieldName <> " of " <> nameBase typeName <> " has no param mapping"
      Just _ -> pure ()

  -- Check every mapping key corresponds to a field
  forM_ (Data.Map.keys mappingMap) \key ->
    case Data.Map.lookup key fieldNameSet of
      Nothing -> fail $ "Mapping key " <> key <> " does not correspond to a field in " <> nameBase typeName
      Just _ -> pure ()

  -- For each mapping, resolve param
  verified <- forM (Data.Map.toList mappingMap) \(fieldNameStr, paramName) ->
    case (Data.Map.lookup fieldNameStr fieldNameSet, Data.Map.lookup paramName paramsByName) of
      (Nothing, _) -> fail $ "Field " <> fieldNameStr <> " not found in " <> nameBase typeName
      (_, Nothing) -> fail $ "Param " <> paramName <> " not found in query"
      (Just fieldName, Just paramIdx) -> pure (fieldName, paramName, paramIdx)

  -- Check all params are covered
  let mappedParamNames = Data.Map.fromList [(snd pair, ()) | pair <- mapping]
  forM_ paramNames \(_ix, mName) ->
    case mName of
      Nothing -> fail "Query has unnamed parameters; embedSqliteMapped requires all params to be named"
      Just nm ->
        let pName = Data.Text.unpack (txtUtf8 nm)
        in unless (Data.Map.member pName mappedParamNames) $
             fail $ "Param " <> pName <> " is not covered by any field mapping"

  -- Sort by param index
  pure $ Data.List.sortOn (\(_, _, ix) -> ix) verified


-- ---------------------------------------------------------------------------
-- Code generation for mapped types
-- ---------------------------------------------------------------------------

-- | Generate a @RowParser@ expression that reads columns in order
-- using @field@ and constructs the user's record type.
generateRowParser :: Name -> [(Name, Database.SQLite3.Direct.ColumnIndex)] -> Q Exp
generateRowParser conName orderedFields = do
  vars <- forM orderedFields \(fieldName, _) -> do
    v <- newName ("v_" <> nameBase fieldName)
    pure (fieldName, v)
  let bindings = [bindS (varP v) [| field |] | (_, v) <- vars]
      recFields = [(fn, VarE v) | (fn, v) <- vars]
      returnStmt = noBindS [| pure $(pure $ RecConE conName recFields) |]
  doE (bindings <> [returnStmt])

-- | Generate a lambda @\\params -> [NamedParam]@ that converts
-- the user's param record into a list of named params for sqlite-simple.
generateSqliteParamList :: [(Name, String, Database.SQLite3.Direct.ParamIndex)] -> Q Exp
generateSqliteParamList orderedParams = do
  paramsVar <- newName "params"
  items <- forM orderedParams \(fieldName, paramName, _) ->
    [| $(stringE paramName) := $(varE fieldName) $(varE paramsVar) |]
  pure $ LamE [VarP paramsVar] (ListE items)


-- ---------------------------------------------------------------------------
-- embedSqliteMapped entry point
-- ---------------------------------------------------------------------------

-- | Generate a query function that works directly with user-defined domain types,
-- without generating intermediate row-types. Uses compile-time mappings to verify
-- that field names match the SQL query's columns and parameters.
embedSqliteMapped
  :: Data.Set.Set FilePath
  -> FilePath
  -> String
  -> Maybe (Name, [(String, String)])
  -> Maybe (Name, [(String, String)])
  -> Q [Dec]
embedSqliteMapped schemas fpQuery name mParams mResults = do
  qAddDependentFile fpQuery
  mapM_ qAddDependentFile (toList schemas)
  let query1 = mkName name
      connName = mkName "conn"
      conn = pure $ VarE connName
      paramsVarName = mkName "params"

  sqliteResult <- runIO $ continueWith schemas fpQuery
  case sqliteResult of
    Left (cs, err) -> fail (Prelude.show cs <> Prelude.show err)
    Right (JustSql _) -> fail "embedSqliteMapped: schema files should not be used with mapped types"
    Right stmt@(SqliteStatement _ (Database.SQLite3.Direct.Utf8 bs) paramNames resultCols) -> do
      txtSql <- decodeUtf8' bs & \case
        Left e -> fail (Prelude.show e)
        Right txt -> pure txt
      runIO $ print stmt

      case (mParams, mResults) of
        -- No params, no results
        (Nothing, Nothing) -> do
          let sig = sigD query1 [t| Connection -> IO () |]
              def = funD query1 [clause [varP connName] (normalB
                [| Database.SQLite.Simple.execute_ $(conn) (Query txtSql) |]) []]
          Data.Traversable.sequence [sig, def]

        -- No params, has results
        (Nothing, Just (resultTypeName, resultMapping)) -> do
          (resultConName, verifiedResults) <- verifySqliteResultMapping resultTypeName resultMapping resultCols
          rowParser <- generateRowParser resultConName verifiedResults
          rowParserName <- newName "rowParser"
          let sig = sigD query1 [t| Connection -> IO [$(conT resultTypeName)] |]
              rowParserDef = valD (varP rowParserName) (normalB (pure rowParser)) []
              def = funD query1 [clause [varP connName] (normalB
                [| Database.SQLite.Simple.queryWith_ $(varE rowParserName) $(conn) (Query txtSql) |]) [rowParserDef]]
          Data.Traversable.sequence [sig, def]

        -- Has params, no results
        (Just (paramTypeName, paramMapping), Nothing) -> do
          verifiedParams <- verifySqliteParamMapping paramTypeName paramMapping paramNames
          paramList <- generateSqliteParamList verifiedParams
          paramListName <- newName "toParams"
          let sig = sigD query1 [t| Connection -> $(conT paramTypeName) -> IO () |]
              paramListDef = valD (varP paramListName) (normalB (pure paramList)) []
              def = funD query1 [clause [varP connName, varP paramsVarName] (normalB
                [| Database.SQLite.Simple.executeNamed $(conn) (Query txtSql) ($(varE paramListName) $(varE paramsVarName)) |]) [paramListDef]]
          Data.Traversable.sequence [sig, def]

        -- Has params and results
        (Just (paramTypeName, paramMapping), Just (resultTypeName, resultMapping)) -> do
          (resultConName, verifiedResults) <- verifySqliteResultMapping resultTypeName resultMapping resultCols
          verifiedParams <- verifySqliteParamMapping paramTypeName paramMapping paramNames
          rowParser <- generateRowParser resultConName verifiedResults
          paramList <- generateSqliteParamList verifiedParams
          rowParserName <- newName "rowParser"
          paramListName <- newName "toParams"
          let sig = sigD query1 [t| Connection -> $(conT paramTypeName) -> IO [$(conT resultTypeName)] |]
              rowParserDef = valD (varP rowParserName) (normalB (pure rowParser)) []
              paramListDef = valD (varP paramListName) (normalB (pure paramList)) []
              def = funD query1 [clause [varP connName, varP paramsVarName] (normalB
                [| Database.SQLite.Simple.queryNamedWith $(varE rowParserName) $(conn) (Query txtSql) ($(varE paramListName) $(varE paramsVarName)) |]) [rowParserDef, paramListDef]]
          Data.Traversable.sequence [sig, def]

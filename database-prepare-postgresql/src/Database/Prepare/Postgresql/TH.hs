{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Prepare.Postgresql.TH (
  -- * Original API
  embedPostgres, executeQuery, parseOrDie,
  PostgresError(..), SchemaIssue(..),
  -- * Mapped API
  embedPostgresMapped, deriveMappingType,
  GFieldPairs(..), fieldPairs,
) where

import Control.Monad
import Database.PostgreSQL.LibPQ
import Database.Prepare.Postgresql.FromField
import Database.Prepare.Postgresql.GetInfo
import Database.Prepare.Postgresql.PgType
import Database.Prepare.Postgresql.ToField
import Data.ByteString
import Data.Char
import Data.List
import Data.Map
import Data.Maybe
import Data.Proxy
import Data.Row.Records
import Data.Text
import Data.Text.Encoding
import Data.Traversable
import GHC.Generics qualified
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Quasi(qAddDependentFile))

data SchemaIssue = ColumnMismatch { shouldBe :: Data.Map.Map Int ComparableColumnInfo, isActually :: Data.Map.Map Int ComparableColumnInfo }
 deriving (Show, Eq)

data PostgresError = NoResultError (Maybe ByteString) | ResultError ExecStatus (Maybe ByteString)
  | SchemaIssue SchemaIssue
 deriving (Show, Eq)

parseOrDie :: String -> Either ParserError b -> b
parseOrDie context = \case
  Left e -> error (context <> ": " <> Prelude.show e)
  Right r -> r

executeQuery :: Connection -> ByteString -> Format -> [Maybe (Oid, ByteString, Format)] -> Data.Map.Map Int ComparableColumnInfo -> (Result -> Database.PostgreSQL.LibPQ.Row -> IO a) -> IO (Either PostgresError [a])
executeQuery conn sql format namedParams expectedFields handleRow  =
      Database.PostgreSQL.LibPQ.execParams conn sql namedParams format
          >>= \case
            Nothing -> do
              e <- errorMessage conn
              pure $ Left (NoResultError e)
            Just res -> do
              status_ <- resultStatus res
              resultErrorMessage res >>= \case
                Just "" -> do
                  numRows <- ntuples res
                  numFields <- nfields res
                  actualFields <- fmap colComparableInfo <$> prepareColumnInfo conn numFields res
                  if numFields /= Database.PostgreSQL.LibPQ.Col (fromIntegral expectedColumns) || actualFields /= expectedFields
                    then pure $ Left (SchemaIssue $ ColumnMismatch expectedFields actualFields)
                    else do
                      Right <$> forM [0..numRows-1] \rowNum -> do
                        handleRow res rowNum
                Just e -> pure $ Left (ResultError status_ (Just e))
                Nothing -> pure $ Left (ResultError status_ Nothing)
      where
          expectedColumns = Data.List.length expectedFields


embedPostgres :: Connection -> FilePath -> String -> Q [Dec]
embedPostgres connection fpQuery name = do
  qAddDependentFile fpQuery
  let query1 = mkName name
  let capName = case name of
         n:ame -> Data.Char.toUpper n:ame
         _ -> name
  sqlite <- runIO do continueWith connection fpQuery
  case sqlite of
        Left (cs, err) -> fail (Prelude.show cs <> Prelude.show err)
        Right stmt@(PostgresStatement _ bs paramInfos expectedFields) -> do
           let connName = mkName "conn"
           let conn = pure $ VarE $ connName
           runIO $ print stmt
           let paramsTyNam = mkName (capName <> "Params")
           let resultsTyNam = mkName (capName <> "Result")
           let paramsVarName = mkName "params"
           buildRowName <- newName "buildRow"
           let proxy = [| Data.Proxy.Proxy |]

           let resultsTy = newtypeD @Q (pure []) resultsTyNam [] Nothing (normalC resultsTyNam [bangTy]) [derivClause Nothing [ ] ]
                  where
                    bangTy = do
                      app <- appT [t|Rec|] recTyArgs
                      pure (Bang NoSourceUnpackedness NoSourceStrictness, app)
                    buildArg :: ColumnInfo -> Q Language.Haskell.TH.Type
                    buildArg columnInfo = [t| $(pure $ LitT $ StrTyLit $ maybe (Prelude.show columnInfo.colComparableInfo.colIndex) (Data.Text.unpack . decodeUtf8) columnInfo.colComparableInfo.colName) .== Database.Prepare.Postgresql.PgType.PgType $(pure $ LitT $ StrTyLit tyName )|]
                       where
                          tyName = Data.Text.unpack columnInfo.colComparableInfo.colSqlType
                    recTyArgs = Data.List.foldr1 (\ty1 ty2 -> infixT ty1 (mkName "Data.Row.Records..+") ty2) (buildArg <$> expectedFields)

           let
                      outFormat = [| Database.PostgreSQL.LibPQ.Text |]
                      buildRowType = sigD buildRowName [t| Result -> Database.PostgreSQL.LibPQ.Row -> IO $(conT resultsTyNam) |]
                      buildRowImplementation = funD buildRowName [clause @Q [varP (mkName "result"),varP (mkName "rowNum") ]  (normalB do
                                [|  do
                                    row <- $(DoE Nothing <$> do
                                       rowName <- newName "row"
                                       empty_ <- [|pure Data.Row.Records.empty|]
                                       buildFields expectedColumns (rowName, empty_, []))
                                    pure $ $(conE resultsTyNam) row |]
                          )  []]
                           where
                             expectedColumns = colComparableInfo <$> Data.Map.elems expectedFields
                             buildFields :: [ComparableColumnInfo] -> (Name, Exp, [Stmt]) -> Q [Stmt]
                             buildFields aa bits = do
                               (_, _, stmts) <- go aa bits
                               pure $ Data.List.reverse stmts
                             go :: [ComparableColumnInfo] -> (Name, Exp, [Stmt]) -> Q (Name, Exp, [Stmt])
                             go [] (theName, theExp, stmts) = pure (theName, theExp, NoBindS theExp:stmts)
                             go (a:aa) (theName, theExp, stmts) = do
                                   nextName <- newName "row"
                                   nextExp <- DoE Nothing <$> do
                                      raw <- newName "raw"
                                      it <- newName "it"
                                      one <- bindS (varP raw) [|getvalue $(varE $ mkName "result") $(varE $ mkName "rowNum") $(litE $ integerL (fromIntegral a.colIndex)) |]
                                      let context = "column " <> labelName <> " (pg type " <> sqlType <> ")"
                                      two <- letS [valD (varP it) (normalB [|parseOrDie $(stringE context) $ fromField $(appTypeE proxy (litT $ strTyLit sqlType)) $(outFormat) (fromJust $(varE raw))|]) []]
                                      three <- noBindS [| pure $  Data.Row.Records.extend $(labelE labelName) $(varE it) $(varE theName) |]
                                      pure [one, two, three]

                                   let nextStmts = BindS (VarP theName) theExp : stmts
                                   go aa (nextName, nextExp, nextStmts)
                               where
                                 labelName = Data.Text.unpack . decodeUtf8 $ fromJust a.colName
                                 sqlType = Data.Text.unpack $ a.colSqlType

           case paramInfos of
                  -- No params, no results (e.g. DELETE without RETURNING)
                  [] | Data.Map.null expectedFields -> do
                      Data.Traversable.sequence [ queryNamedType
                          , queryNamedImplementation
                          ]
                    where
                      queryNamedType = sigD query1 [t| Connection -> IO (Either PostgresError ()) |]
                      expectedFieldsComparable = colComparableInfo <$> expectedFields
                      queryNamedImplementation =
                           let c = clause @Q [pure $ VarP connName] (normalB
                                 [| fmap (fmap (const ())) $ executeQuery $(conn) bs $(outFormat) [] expectedFieldsComparable (\_ _ -> pure ()) |]) []
                           in funD query1 [c]

                  -- No params, has results (e.g. SELECT without WHERE)
                  [] -> do
                      Data.Traversable.sequence [ rowType
                          , resultsTy
                          , buildRowType
                          , buildRowImplementation
                          , queryNamedType
                          , queryNamedImplementation
                          ]
                    where

                      queryNamedType = sigD (query1) [t| Connection -> IO (Either PostgresError [$(pure $ ConT resultsTyNam)]) |]
                      expectedFieldsComparable = colComparableInfo <$> expectedFields
                      queryNamedImplementation =
                           let c = clause @Q [pure $ VarP connName] (normalB
                                 [| executeQuery $(conn) bs $(outFormat) [] expectedFieldsComparable $(varE buildRowName) |]) []
                           in funD query1 [c]
                      rowType = newtypeD @Q (pure []) paramsTyNam [] Nothing (normalC paramsTyNam [bangTy]) [derivClause Nothing [ [t|Show|], [t|Eq|]] ]
                          where
                            bangTy = do
                              app <- recTyArgs
                              pure (Bang NoSourceUnpackedness NoSourceStrictness, app)
                            buildArg :: ParamInfo -> Q Language.Haskell.TH.Type
                            buildArg info = [t| Database.Prepare.Postgresql.PgType.PgType $(pure $ LitT $ StrTyLit tyName ) |]
                              where tyName = Data.Text.unpack info.paramTypeName
                            recTyArgs = Data.List.foldl appT (tupleT (Data.List.length paramInfos)) (buildArg <$> paramInfos)

                  -- Has params, no results (e.g. DELETE with WHERE)
                  _ | Data.Map.null expectedFields -> do
                      Data.Traversable.sequence [ rowType
                          , toNamedParamsTy
                          , toNamedParamsDef
                          , queryNamedType
                          , queryNamedImplementation
                          ]
                    where

                      paramToNamedParam :: ParamInfo -> Name -> Q Exp
                      paramToNamedParam info param = do
                                [|  Just (let (encoded, fmt) = Database.Prepare.Postgresql.ToField.toField ( $(appTypeE proxy (litT $ strTyLit tyname))) $(varE param)
                                         in (Oid $(litE $ integerL oidNum), encoded, fmt)) |]
                              where
                                tyname = Data.Text.unpack info.paramTypeName
                                oidNum = case info.paramOid of Oid n -> fromIntegral n :: Integer

                      toNamedParamsDef  :: Q Dec
                      toNamedParamsDef = funD toNamedParamsVarName [clause @Q [pure $ ConP paramsTyNam [] [VarP paramsVarName]]  (normalB do
                          let eachParam =  paramToNamedParam <$> paramInfos
                          appE (makeListApplier eachParam) (varE paramsVarName)
                          )  []]
                      toNamedParamsVarName = mkName ("toNamed" <> capName <> "Params")
                      toNamedParamsTy :: Q Dec
                      toNamedParamsTy = sigD toNamedParamsVarName [t| $(pure $ ConT paramsTyNam) -> [Maybe (Oid, ByteString, Format)] |]
                      queryNamedType = sigD query1 [t| Connection -> $(pure $ ConT paramsTyNam) -> IO (Either PostgresError ()) |]
                      expectedFieldsComparable = colComparableInfo <$> expectedFields
                      queryNamedImplementation =
                           let c = clause @Q [pure $ VarP connName, pure $ VarP paramsVarName] (normalB
                                 [| fmap (fmap (const ())) $ executeQuery $(conn) bs $(outFormat) ($(varE toNamedParamsVarName) $(varE paramsVarName)) expectedFieldsComparable (\_ _ -> pure ()) |]) []
                           in funD query1 [c]
                      rowType = newtypeD @Q (pure []) paramsTyNam [] Nothing (normalC paramsTyNam [bangTy]) [derivClause Nothing [ [t|Show|], [t|Eq|]] ]
                          where
                            bangTy = do
                              app <- recTyArgs
                              pure (Bang NoSourceUnpackedness NoSourceStrictness, app)
                            buildArg :: ParamInfo -> Q Language.Haskell.TH.Type
                            buildArg info = [t| Database.Prepare.Postgresql.PgType.PgType $(pure $ LitT $ StrTyLit tyName ) |]
                              where tyName = Data.Text.unpack info.paramTypeName
                            recTyArgs = Data.List.foldl appT (tupleT (Data.List.length paramInfos)) (buildArg <$> paramInfos)

                  -- Has params, has results (e.g. INSERT RETURNING, SELECT with WHERE)
                  _ -> do
                      Data.Traversable.sequence [ rowType
                          , toNamedParamsTy
                          , toNamedParamsDef
                          , resultsTy
                          , buildRowType
                          , buildRowImplementation
                          , queryNamedType
                          , queryNamedImplementation
                          ]
                    where

                      paramToNamedParam :: ParamInfo -> Name -> Q Exp
                      paramToNamedParam info param = do
                                [|  Just (let (encoded, fmt) = Database.Prepare.Postgresql.ToField.toField ( $(appTypeE proxy (litT $ strTyLit tyname))) $(varE param)
                                         in (Oid $(litE $ integerL oidNum), encoded, fmt)) |]
                              where
                                tyname = Data.Text.unpack info.paramTypeName
                                oidNum = case info.paramOid of Oid n -> fromIntegral n :: Integer


                      toNamedParamsDef  :: Q Dec
                      toNamedParamsDef = funD toNamedParamsVarName [clause @Q [pure $ ConP paramsTyNam [] [VarP paramsVarName]]  (normalB do
                          let eachParam =  paramToNamedParam <$> paramInfos
                          appE (makeListApplier eachParam) (varE paramsVarName)
                          )  []]
                      toNamedParamsVarName = mkName ("toNamed" <> capName <> "Params")
                      toNamedParamsTy :: Q Dec
                      toNamedParamsTy = sigD toNamedParamsVarName [t| $(pure $ ConT paramsTyNam) -> [Maybe (Oid, ByteString, Format)] |]
                      queryNamedType = sigD (query1) [t| Connection -> $(pure $ ConT paramsTyNam) -> IO (Either PostgresError [$(pure $ ConT resultsTyNam)]) |]
                      expectedFieldsComparable = colComparableInfo <$> expectedFields
                      queryNamedImplementation =
                           let c = clause @Q [pure $ VarP connName, pure $ VarP paramsVarName] (normalB
                                 [| executeQuery $(conn) bs $(outFormat) ($(varE toNamedParamsVarName) $(varE paramsVarName)) expectedFieldsComparable $(varE buildRowName) |]) []
                           in funD query1 [c]
                      rowType = newtypeD @Q (pure []) paramsTyNam [] Nothing (normalC paramsTyNam [bangTy]) [derivClause Nothing [ [t|Show|], [t|Eq|]] ]
                          where
                            bangTy = do
                              app <- recTyArgs
                              pure (Bang NoSourceUnpackedness NoSourceStrictness, app)
                            buildArg :: ParamInfo -> Q Language.Haskell.TH.Type
                            buildArg info = [t| Database.Prepare.Postgresql.PgType.PgType $(pure $ LitT $ StrTyLit tyName ) |]
                              where tyName = Data.Text.unpack info.paramTypeName
                            recTyArgs = Data.List.foldl appT (tupleT (Data.List.length paramInfos)) (buildArg <$> paramInfos)



-- | Given a list of ([Pat], Exp) representing inline lambdas,
-- generate a function that takes a tuple of arguments and applies
-- each lambda to the matching tuple field, collecting results to a list.
makeListApplier :: [(Name -> Q Exp)] -> Q Exp
makeListApplier pes = do
  -- Create variables for each tuple element:
  tupleNames <- replicateM (Data.List.length pes) (newName "a")
  -- Build the tuple pattern (a1, a2, ..., an):
  let tuplePat = TupP (Data.List.map VarP tupleNames)
      -- For each ([pat], body), bind tupleName to pat and apply body:
      apps = Data.List.zipWith (\(mkbody) tupleVar ->
                 mkbody tupleVar )
             pes tupleNames
  -- The result is: \(a1,a2,..,an) -> [f1 a1, f2 a2, ...]
  list_ <- listE apps
  return $ LamE [tuplePat] list_


-- ---------------------------------------------------------------------------
-- Mapped API: deriveMappingType, GFieldPairs, fieldPairs, embedPostgresMapped
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
-- Fails at compile time if the type is not a single-constructor record.
extractRecFields :: Name -> Q (Name, [(Name, Language.Haskell.TH.Type)])
extractRecFields typeName = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ [RecC conName fields] _) -> pure (conName, [(n, t) | (n, _, t) <- fields])
    TyConI (NewtypeD _ _ _ _ (RecC conName fields) _) -> pure (conName, [(n, t) | (n, _, t) <- fields])
    _ -> fail $ nameBase typeName <> " must be a single-constructor record type"

-- | Resolve what Haskell type a PgType maps to for the given SQL type name.
resolvePgType :: String -> Q Language.Haskell.TH.Type
resolvePgType sqlType = do
  insts <- reifyInstances ''PgType [LitT (StrTyLit sqlType)]
  case insts of
    [TySynInstD (TySynEqn _ _ rhs)] -> pure rhs
    _ -> fail $ "No PgType instance for SQL type: " <> sqlType

-- | Verify a result mapping: every field in the Haskell type must map to a column
-- in the query result, types must match, and every column must be covered.
-- Returns fields ordered by column index for codegen.
verifyResultMapping :: Name -> [(String, String)] -> Data.Map.Map Int ColumnInfo -> Q (Name, [(Name, ComparableColumnInfo, String)])
verifyResultMapping typeName mapping columnInfoMap = do
  (conName, fields) <- extractRecFields typeName
  let mappingMap = Data.Map.fromList mapping
      fieldNameSet = Data.Map.fromList [(nameBase n, (n, t)) | (n, t) <- fields]
      -- Build a column lookup by name
      colsByName = Data.Map.fromList
        [(Data.Text.unpack (decodeUtf8 nm), ci.colComparableInfo)
        | ci <- Data.Map.elems columnInfoMap
        , Just nm <- [ci.colComparableInfo.colName]]

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

  -- For each mapping, resolve column and verify type
  verified <- forM (Data.Map.toList mappingMap) \(fieldNameStr, sqlColName) -> do
    case (Data.Map.lookup fieldNameStr fieldNameSet, Data.Map.lookup sqlColName colsByName) of
      (Nothing, _) -> fail $ "Field " <> fieldNameStr <> " not found in " <> nameBase typeName
      (_, Nothing) -> fail $ "SQL column " <> sqlColName <> " not found in query result"
      (Just (fieldName, fieldType), Just colInfo) -> do
        let sqlType = Data.Text.unpack colInfo.colSqlType
        expectedType <- resolvePgType sqlType
        unless (fieldType == expectedType) $
          fail $ "Type mismatch for field " <> nameBase fieldName
            <> ": Haskell type is " <> pprint fieldType
            <> " but SQL column " <> sqlColName <> " (type " <> sqlType
            <> ") maps to " <> pprint expectedType
        pure (fieldName, colInfo, sqlType)

  -- Check all columns are covered
  let mappedColNames = Data.Map.fromList [(snd pair, ()) | pair <- mapping]
  forM_ (Data.Map.elems columnInfoMap) \ci ->
    case ci.colComparableInfo.colName of
      Nothing -> fail "Query has unnamed result column"
      Just nm ->
        unless (Data.Map.member (Data.Text.unpack (decodeUtf8 nm)) mappedColNames) $
          fail $ "SQL column " <> Data.Text.unpack (decodeUtf8 nm) <> " is not covered by any field mapping"

  -- Sort by column index
  pure (conName, Data.List.sortOn (\(_, ci, _) -> ci.colIndex) verified)

-- | Verify a param mapping: every field maps to a valid $N parameter,
-- types must match, and all params must be covered.
-- Returns fields ordered by param index for codegen.
verifyParamMapping :: Name -> [(String, String)] -> [ParamInfo] -> Q [(Name, ParamInfo)]
verifyParamMapping typeName mapping paramInfos = do
  (_conName, fields) <- extractRecFields typeName
  let fieldNameSet = Data.Map.fromList [(nameBase n, (n, t)) | (n, t) <- fields]
      paramByIndex = Data.Map.fromList [(let ParamIndex i = info.paramIndex in i, info) | info <- paramInfos]

  -- Check mapping completeness
  let mappingMap = Data.Map.fromList mapping
  forM_ fields \(fieldName, _) ->
    case Data.Map.lookup (nameBase fieldName) mappingMap of
      Nothing -> fail $ "Field " <> nameBase fieldName <> " of " <> nameBase typeName <> " has no param mapping"
      Just _ -> pure ()
  forM_ (Data.Map.keys mappingMap) \key ->
    case Data.Map.lookup key fieldNameSet of
      Nothing -> fail $ "Mapping key " <> key <> " does not correspond to a field in " <> nameBase typeName
      Just _ -> pure ()

  -- Parse $N and verify
  verified <- forM (Data.Map.toList mappingMap) \(fieldNameStr, dollarN) -> do
    idx <- case dollarN of
      ('$':rest) | [(n, "")] <- reads rest -> pure (n - 1 :: Int)  -- 1-based → 0-based
      _ -> fail $ "Param mapping value " <> dollarN <> " is not a valid $N reference"
    case (Data.Map.lookup fieldNameStr fieldNameSet, Data.Map.lookup idx paramByIndex) of
      (Nothing, _) -> fail $ "Field " <> fieldNameStr <> " not found in " <> nameBase typeName
      (_, Nothing) -> fail $ "Param index $" <> Prelude.show (idx + 1) <> " does not exist in query"
      (Just (fieldName, fieldType), Just info) -> do
        let sqlType = Data.Text.unpack info.paramTypeName
        expectedType <- resolvePgType sqlType
        unless (fieldType == expectedType) $
          fail $ "Type mismatch for param field " <> nameBase fieldName
            <> ": Haskell type is " <> pprint fieldType
            <> " but $" <> Prelude.show (idx + 1) <> " (type " <> sqlType
            <> ") maps to " <> pprint expectedType
        pure (fieldName, info)

  -- Check all params are covered
  let coveredIndices = Data.Map.fromList
        [(idx, ()) | (_, dollarN) <- mapping
        , ('$':rest) <- [dollarN], [(n, "")] <- [reads rest]
        , let idx = n - 1 :: Int]
  forM_ paramInfos \info -> do
    let ParamIndex idx = info.paramIndex
    unless (Data.Map.member idx coveredIndices) $
      fail $ "Param $" <> Prelude.show (idx + 1) <> " is not covered by any field mapping"

  -- Sort by param index
  pure $ Data.List.sortOn (\(_, info) -> info.paramIndex) verified


-- ---------------------------------------------------------------------------
-- Code generation for mapped types
-- ---------------------------------------------------------------------------

-- | Generate a lambda @\\result rowNum -> do { ... ; pure (ConE fields) }@
-- that reads columns from a libpq Result into the user's record type.
generateMappedBuildRow :: Name -> [(Name, ComparableColumnInfo, String)] -> Q Exp
generateMappedBuildRow conName orderedFields = do
  resultVar <- newName "result"
  rowNumVar <- newName "rowNum"
  let proxy = [| Data.Proxy.Proxy |]
      outFormat = [| Database.PostgreSQL.LibPQ.Text |]
  stmts <- forM (Data.List.zip [0 :: Integer ..] orderedFields) \(_, (fieldName, colInfo, sqlType)) -> do
    raw <- newName "raw"
    val <- newName ("v_" <> nameBase fieldName)
    let colIdx = fromIntegral colInfo.colIndex :: Integer
        context = "column " <> Data.Text.unpack (decodeUtf8 (fromJust colInfo.colName))
                  <> " (pg type " <> sqlType <> ")"
    bindRaw <- bindS (varP raw)
      [| getvalue $(varE resultVar) $(varE rowNumVar) $(litE $ integerL colIdx) |]
    letVal <- letS [valD (varP val) (normalB
      [| parseOrDie $(stringE context) $ fromField $(appTypeE proxy (litT $ strTyLit sqlType)) $(outFormat) (fromJust $(varE raw)) |]) []]
    pure (fieldName, val, [bindRaw, letVal])
  let allStmts = Prelude.concatMap (\(_, _, ss) -> ss) stmts
      recFields = [(fieldName, VarE val) | (fieldName, val, _) <- stmts]
      returnStmt = NoBindS $ AppE (VarE 'pure) (RecConE conName recFields)
  pure $ LamE [VarP resultVar, VarP rowNumVar] (DoE Nothing (allStmts <> [returnStmt]))

-- | Generate a lambda @\\params -> [Just (Oid n, encoded, fmt), ...]@
-- that converts the user's param record into a list of encoded params.
generateMappedParamList :: Name -> [(Name, ParamInfo)] -> Q Exp
generateMappedParamList _typeName orderedParams = do
  paramsVar <- newName "params"
  let proxy = [| Data.Proxy.Proxy |]
  items <- forM orderedParams \(fieldName, info) -> do
    let tyname = Data.Text.unpack info.paramTypeName
        oidNum = case info.paramOid of Oid n -> fromIntegral n :: Integer
    [| Just (let (encoded, fmt) = Database.Prepare.Postgresql.ToField.toField ($(appTypeE proxy (litT $ strTyLit tyname))) ($(varE fieldName) $(varE paramsVar))
             in (Oid $(litE $ integerL oidNum), encoded, fmt)) |]
  let body = ListE items
  pure $ LamE [VarP paramsVar] body


-- ---------------------------------------------------------------------------
-- embedPostgresMapped entry point
-- ---------------------------------------------------------------------------

-- | Generate a query function that works directly with user-defined domain types,
-- without generating intermediate row-types. Uses compile-time mappings to verify
-- that field names and types match the SQL query.
embedPostgresMapped
  :: Connection
  -> FilePath
  -> String
  -> Maybe (Name, [(String, String)])
  -> Maybe (Name, [(String, String)])
  -> Q [Dec]
embedPostgresMapped connection fpQuery name mParams mResults = do
  qAddDependentFile fpQuery
  let query1 = mkName name
      connName = mkName "conn"
      conn = pure $ VarE connName
      outFormat = [| Database.PostgreSQL.LibPQ.Text |]

  stmtResult <- runIO $ continueWith connection fpQuery
  case stmtResult of
    Left (cs, err) -> fail (Prelude.show cs <> Prelude.show err)
    Right stmt@(PostgresStatement _ bs paramInfos expectedFields) -> do
      runIO $ print stmt
      let expectedFieldsComparable = colComparableInfo <$> expectedFields

      case (mParams, mResults) of
        -- No params, no results
        (Nothing, Nothing) -> do
          let sig = sigD query1 [t| Connection -> IO (Either PostgresError ()) |]
              def = funD query1 [clause [varP connName] (normalB
                [| fmap (fmap (const ())) $ executeQuery $(conn) bs $(outFormat) [] expectedFieldsComparable (\_ _ -> pure ()) |]) []]
          Data.Traversable.sequence [sig, def]

        -- No params, has results
        (Nothing, Just (resultTypeName, resultMapping)) -> do
          (resultConName, verified) <- verifyResultMapping resultTypeName resultMapping expectedFields
          buildRow <- generateMappedBuildRow resultConName verified
          buildRowName <- newName "buildRow"
          let sig = sigD query1 [t| Connection -> IO (Either PostgresError [$(conT resultTypeName)]) |]
              buildRowDef = valD (varP buildRowName) (normalB (pure buildRow)) []
              def = funD query1 [clause [varP connName] (normalB
                [| executeQuery $(conn) bs $(outFormat) [] expectedFieldsComparable $(varE buildRowName) |]) [buildRowDef]]
          Data.Traversable.sequence [sig, def]

        -- Has params, no results
        (Just (paramTypeName, paramMapping), Nothing) -> do
          verified <- verifyParamMapping paramTypeName paramMapping paramInfos
          paramList <- generateMappedParamList paramTypeName verified
          paramListName <- newName "toParams"
          let paramsVarName = mkName "params"
              sig = sigD query1 [t| Connection -> $(conT paramTypeName) -> IO (Either PostgresError ()) |]
              paramListDef = valD (varP paramListName) (normalB (pure paramList)) []
              def = funD query1 [clause [varP connName, varP paramsVarName] (normalB
                [| fmap (fmap (const ())) $ executeQuery $(conn) bs $(outFormat) ($(varE paramListName) $(varE paramsVarName)) expectedFieldsComparable (\_ _ -> pure ()) |]) [paramListDef]]
          Data.Traversable.sequence [sig, def]

        -- Has params and results
        (Just (paramTypeName, paramMapping), Just (resultTypeName, resultMapping)) -> do
          (resultConName, verifiedResults) <- verifyResultMapping resultTypeName resultMapping expectedFields
          verifiedParams <- verifyParamMapping paramTypeName paramMapping paramInfos
          buildRow <- generateMappedBuildRow resultConName verifiedResults
          paramList <- generateMappedParamList paramTypeName verifiedParams
          buildRowName <- newName "buildRow"
          paramListName <- newName "toParams"
          let paramsVarName = mkName "params"
              sig = sigD query1 [t| Connection -> $(conT paramTypeName) -> IO (Either PostgresError [$(conT resultTypeName)]) |]
              buildRowDef = valD (varP buildRowName) (normalB (pure buildRow)) []
              paramListDef = valD (varP paramListName) (normalB (pure paramList)) []
              def = funD query1 [clause [varP connName, varP paramsVarName] (normalB
                [| executeQuery $(conn) bs $(outFormat) ($(varE paramListName) $(varE paramsVarName)) expectedFieldsComparable $(varE buildRowName) |]) [buildRowDef, paramListDef]]
          Data.Traversable.sequence [sig, def]

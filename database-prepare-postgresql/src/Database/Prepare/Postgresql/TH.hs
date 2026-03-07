{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
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
  -- * API
  embedPostgres, executeQuery, parseOrDie,
  PostgresError(..), SchemaIssue(..),
  deriveMappingType,
  GFieldPairs(..), fieldPairs,
) where

import Control.Monad
import Database.PostgreSQL.LibPQ
import Database.Prepare.Postgresql.FromField
import Database.Prepare.Postgresql.GetInfo
import Database.Prepare.Postgresql.ToField
import Data.ByteString
import Data.List
import Data.Map
import Data.Maybe
import Data.Proxy
import Data.Text
import Data.Text.Encoding
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


-- ---------------------------------------------------------------------------
-- Mapped API: deriveMappingType, GFieldPairs, fieldPairs, embedPostgres
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

-- | Unwrap Maybe from a type, returning (innerType, True) if it was Maybe, (type, False) otherwise.
unwrapMaybe :: Language.Haskell.TH.Type -> (Language.Haskell.TH.Type, Bool)
unwrapMaybe (AppT (ConT n) inner) | n == ''Maybe = (inner, True)
unwrapMaybe t = (t, False)

-- | Verify a result mapping: every field in the Haskell type must map to a column
-- in the query result, types must match, and every column must be covered.
-- Returns fields ordered by column index for codegen.
-- The Bool in the result tuple indicates whether the field is nullable (Maybe).
verifyResultMapping :: Name -> [(String, String)] -> Data.Map.Map Int ColumnInfo -> Q (Name, [(Name, ComparableColumnInfo, String, Bool)])
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
            (_, isNullable) = unwrapMaybe fieldType
        pure (fieldName, colInfo, sqlType, isNullable)

  -- Check all columns are covered
  let mappedColNames = Data.Map.fromList [(snd pair, ()) | pair <- mapping]
  forM_ (Data.Map.elems columnInfoMap) \ci ->
    case ci.colComparableInfo.colName of
      Nothing -> fail "Query has unnamed result column"
      Just nm ->
        unless (Data.Map.member (Data.Text.unpack (decodeUtf8 nm)) mappedColNames) $
          fail $ "SQL column " <> Data.Text.unpack (decodeUtf8 nm) <> " is not covered by any field mapping"

  -- Sort by column index
  pure (conName, Data.List.sortOn (\(_, ci, _, _) -> ci.colIndex) verified)

-- | Verify a param mapping: every field maps to a valid $N parameter,
-- types must match, and all params must be covered.
-- Returns fields ordered by param index for codegen.
-- The Bool in the result tuple indicates whether the param is nullable (Maybe).
verifyParamMapping :: Name -> [(String, String)] -> [ParamInfo] -> Q [(Name, ParamInfo, Bool)]
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
        let (_, isNullable) = unwrapMaybe fieldType
        pure (fieldName, info, isNullable)

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
  pure $ Data.List.sortOn (\(_, info, _) -> info.paramIndex) verified


-- ---------------------------------------------------------------------------
-- Code generation for mapped types
-- ---------------------------------------------------------------------------

-- | Generate a lambda @\\result rowNum -> do { ... ; pure (ConE fields) }@
-- that reads columns from a libpq Result into the user's record type.
-- The Bool in each tuple indicates whether the field is nullable.
generateMappedBuildRow :: Name -> [(Name, ComparableColumnInfo, String, Bool)] -> Q Exp
generateMappedBuildRow conName orderedFields = do
  resultVar <- newName "result"
  rowNumVar <- newName "rowNum"
  let proxy = [| Data.Proxy.Proxy |]
      outFormat = [| Database.PostgreSQL.LibPQ.Text |]
  stmts <- forM orderedFields \(fieldName, colInfo, sqlType, isNullable) -> do
    raw <- newName "raw"
    val <- newName ("v_" <> nameBase fieldName)
    let colIdx = fromIntegral colInfo.colIndex :: Integer
        context = "column " <> Data.Text.unpack (decodeUtf8 (fromJust colInfo.colName))
                  <> " (pg type " <> sqlType <> ")"
    bindRaw <- bindS (varP raw)
      [| getvalue $(varE resultVar) $(varE rowNumVar) $(litE $ integerL colIdx) |]
    letVal <- if isNullable
      then letS [valD (varP val) (normalB
        [| fmap (parseOrDie $(stringE context) . fromField $(appTypeE proxy (litT $ strTyLit sqlType)) $(outFormat)) $(varE raw) |]) []]
      else letS [valD (varP val) (normalB
        [| parseOrDie $(stringE context) $ fromField $(appTypeE proxy (litT $ strTyLit sqlType)) $(outFormat) (fromJust $(varE raw)) |]) []]
    pure (fieldName, val, [bindRaw, letVal])
  let allStmts = Prelude.concatMap (\(_, _, ss) -> ss) stmts
      recFields = [(fieldName, VarE val) | (fieldName, val, _) <- stmts]
      returnStmt = NoBindS $ AppE (VarE 'pure) (RecConE conName recFields)
  pure $ LamE [VarP resultVar, VarP rowNumVar] (DoE Nothing (allStmts <> [returnStmt]))

-- | Generate a lambda @\\params -> [Just (Oid n, encoded, fmt), ...]@
-- that converts the user's param record into a list of encoded params.
-- The Bool in each tuple indicates whether the param is nullable.
generateMappedParamList :: Name -> [(Name, ParamInfo, Bool)] -> Q Exp
generateMappedParamList _typeName orderedParams = do
  paramsVar <- newName "params"
  let proxy = [| Data.Proxy.Proxy |]
  items <- forM orderedParams \(fieldName, info, isNullable) -> do
    let tyname = Data.Text.unpack info.paramTypeName
        oidNum = case info.paramOid of Oid n -> fromIntegral n :: Integer
    if isNullable
      then [| fmap (\v -> let (encoded, fmt) = Database.Prepare.Postgresql.ToField.toField ($(appTypeE proxy (litT $ strTyLit tyname))) v
                           in (Oid $(litE $ integerL oidNum), encoded, fmt)) ($(varE fieldName) $(varE paramsVar)) |]
      else [| Just (let (encoded, fmt) = Database.Prepare.Postgresql.ToField.toField ($(appTypeE proxy (litT $ strTyLit tyname))) ($(varE fieldName) $(varE paramsVar))
                     in (Oid $(litE $ integerL oidNum), encoded, fmt)) |]
  let body = ListE items
  pure $ LamE [VarP paramsVar] body


-- ---------------------------------------------------------------------------
-- embedPostgres entry point
-- ---------------------------------------------------------------------------

-- | Generate a query function that works directly with user-defined domain types.
-- Uses compile-time mappings to verify that field names match the SQL query.
embedPostgres
  :: Connection
  -> FilePath
  -> Maybe (Name, [(String, String)])
  -> Maybe (Name, [(String, String)])
  -> Q Exp
embedPostgres connection fpQuery mParams mResults = do
  qAddDependentFile fpQuery
  let outFormat = [| Database.PostgreSQL.LibPQ.Text |]

  stmtResult <- runIO $ continueWith connection fpQuery
  case stmtResult of
    Left (cs, err) -> fail (Prelude.show cs <> Prelude.show err)
    Right stmt@(PostgresStatement _ bs paramInfos expectedFields) -> do
      runIO $ print stmt
      let expectedFieldsComparable = colComparableInfo <$> expectedFields

      case (mParams, mResults) of
        -- No params, no results
        (Nothing, Nothing) ->
          [| \conn -> fmap (fmap (const ())) $ executeQuery conn bs $(outFormat) [] expectedFieldsComparable (\_ _ -> pure ()) |]

        -- No params, has results
        (Nothing, Just (resultTypeName, resultMapping)) -> do
          (resultConName, verified) <- verifyResultMapping resultTypeName resultMapping expectedFields
          buildRow <- generateMappedBuildRow resultConName verified
          [| \conn -> executeQuery conn bs $(outFormat) [] expectedFieldsComparable $(pure buildRow) |]

        -- Has params, no results
        (Just (paramTypeName, paramMapping), Nothing) -> do
          verified <- verifyParamMapping paramTypeName paramMapping paramInfos
          paramList <- generateMappedParamList paramTypeName verified
          [| \conn params -> fmap (fmap (const ())) $ executeQuery conn bs $(outFormat) ($(pure paramList) params) expectedFieldsComparable (\_ _ -> pure ()) |]

        -- Has params and results
        (Just (paramTypeName, paramMapping), Just (resultTypeName, resultMapping)) -> do
          (resultConName, verifiedResults) <- verifyResultMapping resultTypeName resultMapping expectedFields
          verifiedParams <- verifyParamMapping paramTypeName paramMapping paramInfos
          buildRow <- generateMappedBuildRow resultConName verifiedResults
          paramList <- generateMappedParamList paramTypeName verifiedParams
          [| \conn params -> executeQuery conn bs $(outFormat) ($(pure paramList) params) expectedFieldsComparable $(pure buildRow) |]

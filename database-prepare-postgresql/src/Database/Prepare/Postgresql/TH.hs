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
module Database.Prepare.Postgresql.TH where

import Data.Map
import Database.Prepare.Postgresql.ToField
import Database.Prepare.Postgresql.FromField
import Control.Monad
import Database.PostgreSQL.LibPQ
import Database.Prepare.Postgresql.GetInfo
import Data.Char
import Data.List
import Data.Row.Records
import Data.Traversable
import Data.ByteString
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Quasi(qAddDependentFile))
import Data.Proxy
import Data.Text.Encoding
import Data.Text
import Data.Maybe

data SchemaIssue = ColumnMismatch { shouldBe :: Data.Map.Map Int ComparableColumnInfo, isActually :: Data.Map.Map Int ComparableColumnInfo }
 deriving (Show, Eq)

data PostgresError = NoResultError (Maybe ByteString) | ResultError ExecStatus (Maybe ByteString)
  | SchemaIssue SchemaIssue
 deriving (Show, Eq)

fromRight :: Either a b -> b
fromRight = \case
  Left _e -> error "unexpected left"
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
                  actualFields <- fmap colComparableInfo <$> prepareColumnInfo numFields res
                  if numFields /= Database.PostgreSQL.LibPQ.Col (fromIntegral expectedColumns) || actualFields /= expectedFields
                    then pure $ Left (SchemaIssue $ ColumnMismatch expectedFields actualFields)
                    else do
                      Right <$> forM [0..numRows-1] \rowNum -> do
                        handleRow res rowNum
                Just e -> pure $ Left (ResultError status_ (Just e))
                Nothing -> pure $ Left (ResultError status_ Nothing)
      where
          expectedColumns = Data.List.length expectedFields


embedSqlite :: Connection -> FilePath -> String -> Q [Dec]
embedSqlite connection fpQuery name = do
  qAddDependentFile fpQuery
  let query1 = mkName name
  let capName = case name of
         n:ame -> Data.Char.toUpper n:ame
         _ -> name
  sqlite <- runIO do continueWith connection fpQuery
  case sqlite of
        Left (cs, err) -> fail (Prelude.show cs <> Prelude.show err)
        Right (JustSql bs) -> do
           let connName = mkName "conn"
           let conn = pure $ VarE $ connName
           let c = clause @Q [pure $ VarP connName] (normalB [| Database.PostgreSQL.LibPQ.exec $(conn) bs |]) []
           Data.Traversable.sequence [sigD (query1) [t| Connection -> IO () |]  , funD query1 [c]]
        Right stmt@(SqliteStatement _ bs params expectedFields) -> do
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
                    buildArg columnInfo = [t| $(pure $ LitT $ StrTyLit $ maybe (Prelude.show columnInfo.colComparableInfo.colIndex) (Data.Text.unpack . decodeUtf8) columnInfo.colComparableInfo.colName) .== Database.Prepare.Postgresql.FromField.PgParam $(pure $ LitT $ StrTyLit tyName )|]
                       where
                          tyName = fromMaybe "" $ builtinOids $ fromIntegral oid
                          Oid oid = columnInfo.colType
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
                                      two <- letS [valD (varP it) (normalB [|fromRight $ fromField $(appTypeE proxy (litT $ strTyLit sqlType)) $(outFormat) (fromJust $(varE raw))|]) []]
                                      three <- noBindS [| pure $  Data.Row.Records.extend $(labelE labelName) $(varE it) $(varE theName) |]
                                      pure [one, two, three]

                                   let nextStmts = BindS (VarP theName) theExp : stmts
                                   go aa (nextName, nextExp, nextStmts)
                               where
                                 labelName = Data.Text.unpack . decodeUtf8 $ fromJust a.colName
                                 sqlType = Data.Text.unpack $ a.colSqlType

           case params of
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
                            buildArg :: (ParamIndex, Oid) -> Q Language.Haskell.TH.Type
                            buildArg (_ix, Oid oid) = [t| Database.Prepare.Postgresql.ToField.PgParam $(pure $ LitT $ StrTyLit tyName ) |]
                              where tyName = fromMaybe "" $ builtinOids $ fromIntegral oid
                            recTyArgs = Data.List.foldl appT (tupleT (Data.List.length params)) (buildArg <$> params)
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

                      paramToNamedParam :: ParamIndex -> Oid -> Name -> Q Exp
                      paramToNamedParam _ix (Oid oid) param = do
                                [|  Just (Database.Prepare.Postgresql.ToField.toField ( $(appTypeE proxy (litT $ strTyLit tyname))) $(varE param) ) |]
                              where
                                oid' = fromIntegral oid
                                tyname = fromMaybe "unknown" $ builtinOids oid'


                      toNamedParamsDef  :: Q Dec
                      toNamedParamsDef = funD toNamedParamsVarName [clause @Q [pure $ ConP paramsTyNam [] [VarP paramsVarName]]  (normalB do
                          let eachParam =  (uncurry paramToNamedParam) <$> params
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
                            buildArg :: (ParamIndex, Oid) -> Q Language.Haskell.TH.Type
                            buildArg (_ix, Oid oid) = [t| Database.Prepare.Postgresql.ToField.PgParam $(pure $ LitT $ StrTyLit tyName ) |]
                              where tyName = fromMaybe "" $ builtinOids $ fromIntegral oid
                            recTyArgs = Data.List.foldl appT (tupleT (Data.List.length params)) (buildArg <$> params)




data Field = Field



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

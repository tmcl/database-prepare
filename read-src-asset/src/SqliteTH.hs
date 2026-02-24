{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module SqliteTH where

import Control.Monad.State.Strict
import Data.Char
import Data.Function
import Data.List
import Data.Row.Records
import Data.Set
import Data.Text
import Data.Text.Encoding
import Data.Traversable
import Database.SQLite.Simple
import Database.SQLite.Simple.Internal
import Database.SQLite3.Direct qualified
import GetSqliteInfo
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
      -- let stringName = \case
      --         s | Just rest <- Data.Text.stripPrefix "$" s -> rest
      --           | Just rest <- Data.Text.stripPrefix "@" s -> rest
      --           | Just rest <- Data.Text.stripPrefix "?" s -> "param" <> rest
      --              | otherwise -> s
      -- let buildName ix = \case
      --      Nothing -> "param" <> show ix
      --      Just (Utf8 utf8) -> Data.Text.unpack $ stringName (decodeUtf8 utf8)
      -- let mkField :: ParamIndex -> Maybe Utf8 -> Q (Name, Bang, Type)
      --     mkField ix mName = do
      --         ty <- [t| SQLData |]
      --         pure (mkName $ buildName ix mName, Bang NoSourceUnpackedness NoSourceStrictness, ty)
      -- let recordFields :: [Q VarBangType]  = uncurry mkField <$> params
      let paramsTyNam = mkName (capName <> "Params")
      let resultsTyNam = mkName (capName <> "Result")
      -- let paramsTyNam1 = mkName "Params1"
      let paramsVarName = mkName "params"
      -- let parametersTy = dataD @Q (pure []) paramsTyNam1 [] Nothing [(recC paramsTyNam1  recordFields)] []
      let (parametersTy2, implementation) = case params of
            [] -> ([], [queryUnnamedType, queryUnnamedImplementation])
              where
                queryUnnamedType = sigD (query1) [t|Connection -> IO [$(pure $ ConT resultsTyNam)]|]
                queryUnnamedImplementation =
                  let c = clause @Q [pure $ VarP connName] (normalB [|Database.SQLite.Simple.query_ $(conn) (Query txtSql)|]) []
                   in funD query1 [c]
            _ -> ([rowType, toNamedParamsTy, toNamedParamsDef], [queryNamedType, queryNamedImplementation])
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
                queryNamedType = sigD (query1) [t|Connection -> $(pure $ ConT paramsTyNam) -> IO [$(pure $ ConT resultsTyNam)]|]
                queryNamedImplementation =
                  let c = clause @Q [pure $ VarP connName, pure $ VarP paramsVarName] (normalB [|Database.SQLite.Simple.queryNamed $(conn) (Query txtSql) ($(varE toNamedParamsVarName) $(varE paramsVarName))|]) []
                   in funD query1 [c]
                rowType = newtypeD @Q (pure []) paramsTyNam [] Nothing (normalC paramsTyNam [bangTy]) [derivClause Nothing [[t|Show|], [t|Eq|]]]
                  where
                    bangTy = do
                      app <- appT [t|Rec|] recTyArgs
                      pure (Bang NoSourceUnpackedness NoSourceStrictness, app)
                    buildArg :: (Database.SQLite3.Direct.ParamIndex, Maybe Database.SQLite3.Direct.Utf8) -> Q Type
                    buildArg (ix, mname) = [t|$(pure $ LitT $ StrTyLit $ maybe (Prelude.show ix) (Data.Text.unpack . txtUtf8) mname) .== SQLData|]
                    recTyArgs = Data.List.foldr1 (\ty1 ty2 -> infixT ty1 (mkName "Data.Row.Records..+") ty2) (buildArg <$> params)

      let resultsTy = newtypeD @Q (pure []) resultsTyNam [] Nothing (normalC resultsTyNam [bangTy]) [derivClause Nothing []]
            where
              bangTy = do
                app <- appT [t|Rec|] recTyArgs
                pure (Bang NoSourceUnpackedness NoSourceStrictness, app)
              buildArg :: (Database.SQLite3.Direct.ColumnIndex, Database.SQLite3.Direct.Utf8) -> Q Type
              buildArg (_ix, mname) = [t|$(pure $ LitT $ StrTyLit $ (Data.Text.unpack . txtUtf8) mname) .== Field|]
              recTyArgs = Data.List.foldr1 (\ty1 ty2 -> infixT ty1 (mkName "Data.Row.Records..+") ty2) (buildArg <$> results)

      let rec = mkName "rec"
      let resultsInstance =
            instanceD @Q
              (pure [])
              [t|FromRow $(pure $ ConT resultsTyNam)|]
              [ funD
                  'fromRow
                  [ clause @Q
                      []
                      ( normalB $
                          doE $
                            [ bindS (varP countColumns) [|RP $ lift get|],
                              noBindS [|RP $ lift $ put ($(numCols), [])|],
                              letS [funD columns [clause [] (normalB [|snd $(pure $ VarE countColumns)|]) []]],
                              letS [funD rec [clause [] (normalB builtRecords) []]],
                              noBindS [|pure $ $(pure $ ConE resultsTyNam) $(pure $ VarE rec)|]
                            ]
                      )
                      []
                  ]
              ]
            where
              numCols = litE $ IntegerL $ fromIntegral $ Data.List.length results
              countColumns = mkName "countColumns"
              columns = mkName "columns"
              builtRecords = Data.List.foldr recordBuilder [|Data.Row.Records.empty|] results
              recordBuilder :: (Database.SQLite3.Direct.ColumnIndex, Database.SQLite3.Direct.Utf8) -> Q Exp -> Q Exp
              recordBuilder (Database.SQLite3.Direct.ColumnIndex ix, utf8) rest =
                let fieldName = strUtf8 utf8
                    ixE = litE $ IntegerL $ fromIntegral ix
                 in [|$(appTypeE (conE 'Label) (pure $ LitT $ StrTyLit fieldName)) .== Field ($(varE columns) !! $(ixE)) $(ixE) .+ $rest|]

      Data.Traversable.sequence (parametersTy2 <> [resultsTy, resultsInstance] <> implementation)

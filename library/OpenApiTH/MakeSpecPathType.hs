module OpenApiTH.MakeSpecPathType where

import Prelude

import Language.Haskell.TH
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KeyMap

import OpenApiTH.DataType
import OpenApiTH.Spec
import OpenApiTH.SpecPath
import OpenApiTH.ResolveSpecPath

makeSpecPathSchemaType :: (MonadFail m, Quote m) => Spec -> SpecPath -> m ()
makeSpecPathSchemaType spec specPath = do
  schemaValue <- resolveSpecPath spec specPath
  dataType <- schemaValueType schemaValue
  _

schemaValueType :: MonadFail m => JSON.Value -> m DataType
schemaValueType = \case
  JSON.Object o -> case KeyMap.lookup "type" o of
    Just x -> case x of
      JSON.String y -> textDataType y
      _ -> fail "`type` is not a string"
    Nothing -> fail "Missing `type` field"
  _ -> fail "Not an object"

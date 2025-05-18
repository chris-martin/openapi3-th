module OpenApiTH.Schema where

import Prelude

import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as KeyMap
import Language.Haskell.TH

import Control.Monad
import OpenApiTH.DataType

data Schema = Schema {value ∷ JSON.Value}

getSchemaObject ∷ MonadFail m ⇒ Schema → m JSON.Object
getSchemaObject schema = case schema.value of
  JSON.Object o → pure o
  _ → fail "Not an object"

getSchemaDataType ∷ MonadFail m ⇒ Schema → m DataType
getSchemaDataType =
  getSchemaObject >=> \o →
    case KeyMap.lookup "type" o of
      Just x → case x of
        JSON.String y → textDataType y
        _ → fail "`type` is not a string"
      Nothing → fail "Missing `type` field"

data SchemaTypeOptions = SchemaTypeOptions
  { name ∷ Maybe String
  }

defaultSchemaTypeOptions ∷ SchemaTypeOptions
defaultSchemaTypeOptions = SchemaTypeOptions {name = Nothing}

makeSchemaType ∷ (Quote m, MonadFail m) ⇒ Schema → SchemaTypeOptions → m Type
makeSchemaType schema opt = do
  name ← case opt.name of
    Nothing → fail "todo"
    Just x → pure x
  dataType ← getSchemaDataType schema
  case dataType of
    Array → do
      itemsSchema ← getSchemaItemsSchema schema
      [t|()|]
    Null → fail "todo"
    Boolean → fail "todo"
    Object → fail "todo"
    Number → fail "todo"
    String → fail "todo"
    Integer → fail "todo"

getSchemaItemsSchema ∷ MonadFail m ⇒ Schema → m Schema
getSchemaItemsSchema =
  getSchemaObject >=> \o →
    case KeyMap.lookup "items" o of
      Just x → pure $ Schema x
      Nothing → fail "Missing `items` field"

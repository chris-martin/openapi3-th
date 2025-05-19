-- |
--
-- https://swagger.io/specification/#data-types
module OpenApiTH.DataType where

import Essentials

import Control.Arrow
import Control.Monad.Fail
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String
import Data.Text (Text)
import Prelude (Bounded (..))

data DataType
  = Null
  | Boolean
  | Object
  | Array
  | Number
  | String
  | Integer
  deriving stock (Enum, Bounded)

dataTypeString ∷ IsString a ⇒ DataType → a
dataTypeString = \case
  Null → "null"
  Boolean → "boolean"
  Object → "object"
  Array → "array"
  Number → "number"
  String → "string"
  Integer → "integer"

textDataTypeMap ∷ Map Text DataType
textDataTypeMap =
  Map.fromList $ (dataTypeString &&& id) <$> [minBound .. maxBound]

textDataTypeMaybe ∷ Text → Maybe DataType
textDataTypeMaybe x = Map.lookup x textDataTypeMap

textDataType ∷ MonadFail m ⇒ Text → m DataType
textDataType = maybe (fail "Unrecognized type") pure . textDataTypeMaybe

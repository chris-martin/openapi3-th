-- | <https://datatracker.ietf.org/doc/html/rfc6901>
module Oath.JsonPointer where

import Essentials

import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types
import Data.Int (Int)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Vector qualified as V

newtype JsonPointer = JsonPointer (Seq Text)

-- jsonPointerGrammar

-- | https://www.rfc-editor.org/rfc/rfc6901#section-4
evaluateJsonPointer ∷ JsonPointer → Value → Maybe Value
evaluateJsonPointer =
  \(JsonPointer items) → go items
 where
  go Empty = Just
  go (x :<| xs) = go xs <=< evaluateJsonReferenceToken x

evaluateJsonReferenceToken ∷ Text → Value → Maybe Value
evaluateJsonReferenceToken x = \case
  JSON.Object m → KeyMap.lookup (Key.fromText x) m
  JSON.Array a → tokenAsIndex x >>= (a V.!?)
  _ → Nothing

tokenAsIndex ∷ Text → Maybe Int
tokenAsIndex = _

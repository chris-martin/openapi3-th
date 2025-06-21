-- | <https://datatracker.ietf.org/doc/html/rfc6901>
module OpenApiTH.Web.JsonPointer where

import Essentials

import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types
import Data.Sequence (Seq (..))
import Data.Text (Text)

newtype JsonPointer = JsonPointer (Seq Key)

resolveJsonPointer ∷ JsonPointer → Value → Maybe Value
resolveJsonPointer =
  \(JsonPointer items) → go items
 where
  go Empty = Just
  go (x :<| xs) = \case
    JSON.Object z → KeyMap.lookup x z >>= go xs
    _ → Nothing

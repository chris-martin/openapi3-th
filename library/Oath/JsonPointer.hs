-- | <https://datatracker.ietf.org/doc/html/rfc6901>
module Oath.JsonPointer where

import Essentials

import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types
import Data.Char (Char)
import Data.Int (Int)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Optics hiding (Empty)

import Oath.Grammar
import Numeric.Natural (Natural)

jsonPointerGrammar ∷ Grammar Text (Seq Text)
jsonPointerGrammar =
  label "json-pointer" $
    seqGrammar $
      constGrammar "/" +> referenceTokenGrammar

referenceTokenGrammar ∷ Grammar Text Text
referenceTokenGrammar = label "reference-token" $
  isoGrammar (iso T.unpack T.pack) $
    listGrammar $
      grammarAlternatives
        [ unescapedGrammar
        , escapedGrammar
        ]

unescapedGrammar ∷ Grammar Text Char
unescapedGrammar = label "unescaped" _

escapedGrammar ∷ Grammar Text Char
escapedGrammar = label "escaped" _

-- | https://www.rfc-editor.org/rfc/rfc6901#section-4
evaluateJsonPointer ∷ Seq Text → Value → Maybe Value
evaluateJsonPointer = go
 where
  go = \case
    Empty → Just
    (x :<| xs) → go xs <=< evaluateJsonReferenceToken x

evaluateJsonReferenceToken ∷ Text → Value → Maybe Value
evaluateJsonReferenceToken x = \case
  JSON.Object m → KeyMap.lookup (Key.fromText x) m
  JSON.Array a → tokenAsIndex x >>= (a V.!?)
  _ → Nothing

tokenAsIndex ∷ Text → Maybe Int
tokenAsIndex = _

arrayIndexGrammar :: Grammar Text Natural
arrayIndexGrammar = _

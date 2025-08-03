-- | <https://datatracker.ietf.org/doc/html/rfc6901>
module Oath.JsonPointer where

import Essentials

import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types
import Data.Char (Char, chr, ord)
import Data.Foldable1 (foldl1')
import Data.Int (Int)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Numeric.Natural (Natural)
import Optics hiding (Empty)
import Prelude (fromIntegral, (*), (+), (-))

import Oath.Grammar

jsonPointerGrammar ∷ Grammar Text (Seq Text)
jsonPointerGrammar =
  label "json-pointer" $
    seqGrammar $
      constGrammar "/" +> referenceTokenGrammar

referenceTokenGrammar ∷ Grammar Text Text
referenceTokenGrammar =
  label "reference-token" $
    isoGrammar (iso T.unpack T.pack) $
      listGrammar $
        grammarAlternatives
          [ unescapedGrammar
          , escapedGrammar
          ]

unescapedGrammar ∷ Grammar Text Char
unescapedGrammar =
  label "unescaped" $
    grammarAlternatives
      [ tokenRange (chr 0x00, chr 0x2E)
      , tokenRange (chr 0x30, chr 0x7D)
      , tokenRange (chr 0x7F, chr 0x10FFFF)
      ]

escapedGrammar ∷ Grammar Text Char
escapedGrammar =
  label "escaped" $
    constGrammar "~"
      +> grammarAlternatives
        [ prismGrammar (only '~') $ constGrammar "0"
        , prismGrammar (only '/') $ constGrammar "1"
        ]

-- | https://www.rfc-editor.org/rfc/rfc6901#section-4
evaluateJsonPointer ∷ Seq Text → Value → Maybe Value
evaluateJsonPointer = go
 where
  go = \case
    Empty → Just
    (x :<| xs) → go xs <=< evaluateReferenceToken x

evaluateReferenceToken ∷ Text → Value → Maybe Value
evaluateReferenceToken x = \case
  JSON.Object m → KeyMap.lookup (Key.fromText x) m
  JSON.Array a → tokenAsIndex x >>= (a V.!?)
  _ → Nothing

tokenAsIndex ∷ Text → Maybe Int
tokenAsIndex = _

arrayIndexGrammar ∷ Grammar Text Natural
arrayIndexGrammar =
  label "array-index" $
    grammarAlternatives
      [ prismGrammar (only 0) $ constGrammar "0"
      , prismGrammar
          ( prism'
              ( foldl1' (\t x → (t * 10) + x)
                  . fmap (\c → fromIntegral $ ord c - ord '0')
                  . (\(x :& xs) → x :| xs)
              )
              _
          )
          $ tokenRange ('1', '9') <+> listGrammar (tokenRange ('0', '9'))
      ]

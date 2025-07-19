module Oath.Uri.Rfc3986.Grammar.Segment (
  segmentGrammar,
  segmentNzGrammar,
  segmentNzNcGrammar,
) where

import Essentials

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.List.NonEmpty (nonEmpty)
import Optics

import Oath.Abnf.Rfc2234
import Oath.Grammar
import Oath.Uri.Rfc3986.Grammar.Characters

segmentGrammar ∷ Grammar ByteString
segmentGrammar =
  label "segment" $
    isoGrammar (iso BS.unpack BS.pack) $
      listGrammar pcharGrammar

segmentNzGrammar ∷ Grammar ByteString
segmentNzGrammar =
  label "segment-nz" $
    prismGrammar (prism' (BS.pack . toList) (nonEmpty . BS.unpack)) $
      list1Grammar pcharGrammar

segmentNzNcGrammar ∷ Grammar ByteString
segmentNzNcGrammar =
  label "segment-nz-nc" $
    prismGrammar (prism' (BS.pack . toList) (nonEmpty . BS.unpack)) $
      list1Grammar $
        grammarAlternatives
          [ unreservedGrammar
          , subDelimGrammar
          , tokenEnumeration $ char <$> "@"
          , pctEncodedGrammar
          ]

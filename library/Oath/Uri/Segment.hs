module Oath.Uri.Segment (
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

import Oath.Abnf
import Oath.Grammar
import Oath.Uri.Characters

segmentGrammar ∷ Grammar ByteString ByteString
segmentGrammar =
  label "segment" $
    isoGrammar (iso BS.unpack BS.pack) $
      listGrammar pcharGrammar

segmentNzGrammar ∷ Grammar ByteString ByteString
segmentNzGrammar =
  label "segment-nz" $
    prismGrammar (prism' (BS.pack . toList) (nonEmpty . BS.unpack)) $
      list1Grammar pcharGrammar

segmentNzNcGrammar ∷ Grammar ByteString ByteString
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

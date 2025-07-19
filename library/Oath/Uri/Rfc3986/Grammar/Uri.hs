module Oath.Uri.Rfc3986.Grammar.Uri where

import Essentials

import Data.ByteString (ByteString)
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty (NESeq (..))
import Optics
import Test.QuickCheck.Arbitrary.Generic

import Oath.Grammar
import Oath.Uri.Rfc3986.Grammar.Appendages
import Oath.Uri.Rfc3986.Grammar.Authority
import Oath.Uri.Rfc3986.Grammar.Path
import Oath.Uri.Rfc3986.Grammar.Scheme

data Uri = Uri
  { scheme ∷ ByteString
  , hierPart ∷ HierPart
  , query ∷ Maybe ByteString
  , fragment ∷ Maybe ByteString
  }

-- | 'Uri' without a fragment
--
-- <https://www.rfc-editor.org/rfc/rfc3986#section-4.3>
data AbsoluteUri = AbsoluteUri
  { scheme ∷ ByteString
  , hierPart ∷ HierPart
  , query ∷ Maybe ByteString
  }

data HierPart
  = HierPart_Authority Authority (Seq ByteString)
  | HierPart_Absolute (Seq ByteString)
  | HierPart_Rootless (NESeq ByteString)
  | HierPart_Empty

makeFieldLabels ''Uri
makeFieldLabels ''AbsoluteUri
makePrismLabels ''HierPart

instance HasGrammar Uri where
  grammar =
    label "URI"
      $ isoGrammar
        ( iso
            ( \Uri {scheme, hierPart, query, fragment} →
                scheme :& hierPart :& query :& fragment
            )
            ( \(scheme :& hierPart :& query :& fragment) →
                Uri {scheme, hierPart, query, fragment}
            )
        )
      $ (schemeGrammar <+ constGrammar ":")
        <+> grammar
        <+> optionalGrammar (constGrammar "?" +> queryGrammar)
        <+> optionalGrammar (constGrammar "#" +> fragmentGrammar)

instance HasGrammar AbsoluteUri where
  grammar =
    label "absolute-uri"
      $ isoGrammar
        ( iso
            ( \AbsoluteUri {scheme, hierPart, query} →
                scheme :& hierPart :& query
            )
            ( \(scheme :& hierPart :& query) →
                AbsoluteUri {scheme, hierPart, query}
            )
        )
      $ (schemeGrammar <+ constGrammar ":")
        <+> grammar
        <+> optionalGrammar (constGrammar "?" +> queryGrammar)

instance HasGrammar HierPart where
  grammar =
    label "hier-part" $
      grammarAlternatives
        [ prismGrammar
            ( #_HierPart_Authority
                % iso
                  (\(authority, path) → authority :& path)
                  (\(authority :& path) → (authority, path))
            )
            $ grammar <+> pathAbemptyGrammar
        , prismGrammar #_HierPart_Absolute pathAbsoluteGrammar
        , prismGrammar #_HierPart_Rootless pathRootlessGrammar
        , prismGrammar #_HierPart_Empty pathEmptyGrammar
        ]

deriving via
  TheGrammar Uri
  instance
    Arbitrary Uri

deriving via
  TheGrammar AbsoluteUri
  instance
    Arbitrary AbsoluteUri

deriving via
  TheGrammar HierPart
  instance
    Arbitrary HierPart

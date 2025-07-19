module Oath.Uri.Rfc3986.Grammar.RelativeRef where

import Essentials

import Data.ByteString (ByteString)
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty (NESeq (..))
import GHC.Generics
import Optics
import Test.QuickCheck.Arbitrary.Generic

import Oath.Grammar
import Oath.Uri.Rfc3986.Grammar.Appendages
import Oath.Uri.Rfc3986.Grammar.Authority
import Oath.Uri.Rfc3986.Grammar.Path

data RelativeRef = RelativeRef
  { relativePart ∷ RelativePart
  , query ∷ Maybe ByteString
  , fragment ∷ Maybe ByteString
  }
  deriving stock Generic

data RelativePart
  = RelativePart_Authority Authority (Seq ByteString)
  | RelativePart_Absolute (Seq ByteString)
  | RelativePart_Noscheme (NESeq ByteString)
  | RelativePart_Empty
  deriving stock Generic

makeFieldLabels ''RelativeRef
makePrismLabels ''RelativePart

instance HasGrammar RelativeRef where
  grammar =
    label "relative-ref"
      $ isoGrammar
        ( iso
            ( \RelativeRef {relativePart, query, fragment} →
                relativePart :& query :& fragment
            )
            ( \(relativePart :& query :& fragment) →
                RelativeRef {relativePart, query, fragment}
            )
        )
      $ grammar
        <+> optionalGrammar (constGrammar "?" +> queryGrammar)
        <+> optionalGrammar (constGrammar "#" +> fragmentGrammar)

instance HasGrammar RelativePart where
  grammar =
    label "relative-part" $
      grammarAlternatives
        [ prismGrammar
            ( #_RelativePart_Authority
                % iso
                  (\(authority, path) → authority :& path)
                  (\(authority :& path) → (authority, path))
            )
            $ constGrammar "//" +> grammar <+> pathAbemptyGrammar
        , prismGrammar #_RelativePart_Absolute pathAbsoluteGrammar
        , prismGrammar #_RelativePart_Noscheme pathNoschemeGrammar
        , prismGrammar #_RelativePart_Empty pathEmptyGrammar
        ]

deriving via
  TheGrammar RelativeRef
  instance
    Arbitrary RelativeRef

deriving via
  TheGrammar RelativePart
  instance
    Arbitrary RelativePart

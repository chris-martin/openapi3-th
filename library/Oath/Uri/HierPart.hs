module Oath.Uri.HierPart (
  HierPart (..),
  hierPartGrammar,
  relativePartGrammar,
) where

import Essentials

import Data.ByteString (ByteString)
import Data.Either (Either (..))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Optics

import Oath.Grammar
import Oath.Uri.Authority (Authority (..), authorityGrammar)
import Oath.Uri.Path

-- | Consisting of a path and an optional authority, 'HierPart' is a
--   constituent of URIs, URI references, and base URIs.
--
-- Corresponds to either
-- <hier-part https://www.rfc-editor.org/rfc/rfc3986#section-3> or
-- <relative-part https://www.rfc-editor.org/rfc/rfc3986#section-4.2>
data HierPart
  = HierPart_Authority Authority (Seq ByteString)
  | HierPart_Absolute (Seq ByteString)
  | HierPart_Relative (Seq ByteString)

makePrismLabels ''HierPart

instance LabelOptic "absolute" A_Getter HierPart HierPart Bool Bool where
  labelOptic = to \case
    HierPart_Authority _ _ -> True
    HierPart_Absolute _ -> True
    HierPart_Relative _ -> False

instance LabelOptic "path" A_Lens HierPart HierPart (Seq ByteString) (Seq ByteString) where
  labelOptic =
    lens
      ( \case
          HierPart_Authority _ x → x
          HierPart_Absolute x → x
          HierPart_Relative x → x
      )
      ( \case
          HierPart_Authority a _ → \x → HierPart_Authority a x
          HierPart_Absolute _ → HierPart_Absolute
          HierPart_Relative _ → HierPart_Relative
      )

instance LabelOptic "authority" An_AffineTraversal HierPart HierPart Authority Authority where
  labelOptic = #_HierPart_Authority % _1

hierPartGrammar ∷ Grammar HierPart
hierPartGrammar =
  label "hier-part" $
    grammarAlternatives
      [ prismGrammar
          ( #_HierPart_Authority
              % iso
                (\(authority, path) → authority :& path)
                (\(authority :& path) → (authority, path))
          )
          $ authorityGrammar <+> pathAbemptyGrammar
      , prismGrammar #_HierPart_Absolute pathAbsoluteGrammar
      , prismGrammar (#_HierPart_Relative % neSeq) pathRootlessGrammar
      , prismGrammar (#_HierPart_Relative % only Seq.Empty) pathEmptyGrammar
      ]

relativePartGrammar ∷ Grammar HierPart
relativePartGrammar =
  label "relative-part" $
    grammarAlternatives
      [ prismGrammar
          ( #_HierPart_Authority
              % iso
                (\(authority, path) → authority :& path)
                (\(authority :& path) → (authority, path))
          )
          $ constGrammar "//" +> authorityGrammar <+> pathAbemptyGrammar
      , prismGrammar #_HierPart_Absolute pathAbsoluteGrammar
      , prismGrammar (#_HierPart_Relative % neSeq) pathNoschemeGrammar
      , prismGrammar (#_HierPart_Relative % only Seq.Empty) pathEmptyGrammar
      ]

neSeq ∷ Prism (Seq a) (Seq b) (NESeq a) (NESeq b)
neSeq = prism NESeq.toSeq \case
  NESeq.IsEmpty → Left Seq.Empty
  NESeq.IsNonEmpty xs → Right xs

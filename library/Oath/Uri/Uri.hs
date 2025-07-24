module Oath.Uri.Uri where

import Essentials

import Data.ByteString (ByteString)
import Data.Sequence (Seq)
import GHC.Generics
import Optics
import Test.QuickCheck.Arbitrary.Generic

import Oath.Grammar
import Oath.Uri.Appendages
import Oath.Uri.Authority
import Oath.Uri.HierPart
import Oath.Uri.Scheme

data Uri = Uri
  { scheme ∷ ByteString
  , hierPart ∷ HierPart
  , query ∷ Maybe ByteString
  , fragment ∷ Maybe ByteString
  }
  deriving stock (Eq, Show, Generic)

makeFieldLabels ''Uri

instance LabelOptic "authority" An_AffineTraversal Uri Uri Authority Authority where
  labelOptic = #hierPart % #authority

instance LabelOptic "path" A_Lens Uri Uri (Seq ByteString) (Seq ByteString) where
  labelOptic = #hierPart % #path

uriGrammar ∷ Grammar Uri
uriGrammar =
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
      <+> hierPartGrammar
      <+> optionalGrammar (constGrammar "?" +> queryGrammar)
      <+> optionalGrammar (constGrammar "#" +> fragmentGrammar)

instance Arbitrary Uri where
  arbitrary = uriGrammar.generator


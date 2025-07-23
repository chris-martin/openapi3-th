module Oath.Uri.AbsoluteUri where

import Essentials

import Data.ByteString (ByteString)
import GHC.Generics
import Optics
import Test.QuickCheck (Arbitrary (..))

import Oath.Grammar
import Oath.Uri.Appendages
import Oath.Uri.Authority
import Oath.Uri.HierPart
import Oath.Uri.Scheme

-- | 'Uri' without a fragment
--
-- <https://www.rfc-editor.org/rfc/rfc3986#section-4.3>
data AbsoluteUri = AbsoluteUri
  { scheme ∷ ByteString
  , hierPart ∷ HierPart
  , query ∷ Maybe ByteString
  }
  deriving stock (Generic, Eq, Show)

makeFieldLabels ''AbsoluteUri

instance LabelOptic "authority" An_AffineTraversal AbsoluteUri AbsoluteUri Authority Authority where
  labelOptic = #hierPart % #_HierPart_Authority % _1

absoluteUriGrammar ∷ Grammar AbsoluteUri
absoluteUriGrammar =
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
      <+> hierPartGrammar
      <+> optionalGrammar (constGrammar "?" +> queryGrammar)

instance Arbitrary AbsoluteUri where
  arbitrary = absoluteUriGrammar.generator

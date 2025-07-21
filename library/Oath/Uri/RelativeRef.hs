module Oath.Uri.RelativeRef where

import Essentials

import Data.ByteString (ByteString)
import GHC.Generics
import Optics
import Test.QuickCheck.Arbitrary.Generic

import Oath.Grammar
import Oath.Uri.Appendages
import Oath.Uri.HierPart

data RelativeRef = RelativeRef
  { hierPart ∷ HierPart
  , query ∷ Maybe ByteString
  , fragment ∷ Maybe ByteString
  }
  deriving stock Generic

makeFieldLabels ''RelativeRef

relativeRefGrammar ∷ Grammar RelativeRef
relativeRefGrammar =
  label "relative-ref"
    $ isoGrammar
      ( iso
          ( \RelativeRef {hierPart, query, fragment} →
              hierPart :& query :& fragment
          )
          ( \(hierPart :& query :& fragment) →
              RelativeRef {hierPart, query, fragment}
          )
      )
    $ relativePartGrammar
      <+> optionalGrammar (constGrammar "?" +> queryGrammar)
      <+> optionalGrammar (constGrammar "#" +> fragmentGrammar)

instance Arbitrary RelativeRef where
  arbitrary = relativeRefGrammar.generator

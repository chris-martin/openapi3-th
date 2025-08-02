module Oath.Uri.UriReference where

import Essentials

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.Maybe (isNothing)
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Optics
import Test.QuickCheck.Arbitrary.Generic

import Oath.Grammar
import Oath.Uri.Authority
import Oath.Uri.HierPart
import Oath.Uri.RelativeRef
import Oath.Uri.Uri

-- | <https://www.rfc-editor.org/rfc/rfc3986#section-4.1>
data UriReference = UriReference
  { scheme ∷ Maybe ByteString
  , hierPart ∷ HierPart
  , query ∷ Maybe ByteString
  , fragment ∷ Maybe ByteString
  }
  deriving stock (Generic, Eq, Show)

makeFieldLabels ''UriReference

instance LabelOptic "scheme" An_AffineTraversal UriReference UriReference ByteString ByteString where
  labelOptic = #scheme % _Just

instance LabelOptic "path" A_Lens UriReference UriReference (Seq ByteString) (Seq ByteString) where
  labelOptic = #hierPart % #path

instance LabelOptic "authority" An_AffineTraversal UriReference UriReference Authority Authority where
  labelOptic = #hierPart % #authority

uriReferenceUriPrism ∷ Prism' UriReference Uri
uriReferenceUriPrism =
  prism'
    ( \Uri {scheme, hierPart, query, fragment} →
        UriReference {scheme = Just scheme, hierPart, query, fragment}
    )
    ( \UriReference {scheme = schemeMaybe, hierPart, query, fragment} → do
        scheme ← schemeMaybe
        pure Uri {scheme, hierPart, query, fragment}
    )
makePrismLabels ''UriReference

uriReferenceRelativeRefPrism ∷ Prism' UriReference RelativeRef
uriReferenceRelativeRefPrism =
  prism'
    ( \RelativeRef {hierPart, query, fragment} →
        UriReference {scheme = Nothing, hierPart, query, fragment}
    )
    ( \UriReference {scheme = schemeMaybe, hierPart, query, fragment} → do
        guard $ isNothing schemeMaybe
        pure RelativeRef {hierPart, query, fragment}
    )

uriReferenceGrammar ∷ Grammar ByteString UriReference
uriReferenceGrammar =
  label "URI-reference" $
    grammarAlternatives
      [ prismGrammar uriReferenceUriPrism uriGrammar
      , prismGrammar uriReferenceRelativeRefPrism relativeRefGrammar
      ]

instance Arbitrary UriReference where
  arbitrary = uriReferenceGrammar.generator

readUriReferenceMaybe ∷ ByteString → Maybe UriReference
readUriReferenceMaybe = readGrammarMaybe uriReferenceGrammar

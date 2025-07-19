module Oath.Uri.Rfc3986.Grammar.UriReference where

import Essentials

import Optics
import Test.QuickCheck.Arbitrary.Generic

import Oath.Grammar
import Oath.Uri.Rfc3986.Grammar.RelativeRef
import Oath.Uri.Rfc3986.Grammar.Uri

-- | <https://www.rfc-editor.org/rfc/rfc3986#section-4.1>
data UriReference
  = UriReference_Uri Uri
  | UriReference_RelativeRef RelativeRef

makePrismLabels ''UriReference

instance HasGrammar UriReference where
  grammar =
    label "URI-reference" $
      grammarAlternatives
        [ prismGrammar #_UriReference_Uri grammar
        , prismGrammar #_UriReference_RelativeRef grammar
        ]

deriving via
  TheGrammar UriReference
  instance
    Arbitrary UriReference

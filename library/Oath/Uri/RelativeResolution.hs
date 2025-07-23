-- | <https://www.ietf.org/rfc/rfc3986.txt>
--   Section 5.2, Relative Resolution
module Oath.Uri.RelativeResolution (
  resolveUriReference,
) where

import Essentials

import Control.Applicative ((<|>))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Optics

import Oath.Uri.AbsoluteUri
import Oath.Uri.HierPart
import Oath.Uri.Path
import Oath.Uri.Uri
import Oath.Uri.UriReference

-- | Section 5.2.2, Transform References
resolveUriReference ∷ AbsoluteUri → UriReference → Uri
resolveUriReference base = \case
  ref@UriReference {scheme = Just scheme} →
    Uri
      { scheme
      , hierPart = ref.hierPart & #path %~ removeDotSegments
      , query = ref.query
      , fragment = ref.fragment
      }
  ref@UriReference {hierPart = HierPart_Authority authority path} →
    Uri
      { scheme = base.scheme
      , hierPart = HierPart_Authority authority $ removeDotSegments path
      , query = ref.query
      , fragment = ref.fragment
      }
  ref@UriReference {hierPart = HierPart_Relative Seq.Empty} →
    Uri
      { scheme = base.scheme
      , hierPart = base.hierPart
      , query = ref.query <|> base.query
      , fragment = ref.fragment
      }
  ref@UriReference {hierPart = HierPart_Absolute path} →
    Uri
      { scheme = base.scheme
      , hierPart = absoluteHierPart (base ^? #authority) (removeDotSegments path)
      , query = ref.query
      , fragment = ref.fragment
      }
  ref@UriReference {hierPart = HierPart_Relative refPath} →
    Uri
      { scheme = base.scheme
      , hierPart =
          base.hierPart
            & #path
              %~ ( \case
                     Seq.Empty → refPath
                     xs :|> _ → xs <> refPath
                 )
      , query = ref.query
      , fragment = ref.fragment
      }

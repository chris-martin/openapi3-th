-- | <https://www.ietf.org/rfc/rfc3986.txt>
--   Section 5.2, Relative Resolution
module Oath.Uri.RelativeResolution (
  resolveUriReference,
) where

import Essentials

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Either (Either (..))
import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text (Text)
import Optics
import Optics.Core.Extras (is)

import Oath.Uri.AbsoluteUri
import Oath.Uri.Authority
import Oath.Uri.HierPart
import Oath.Uri.Path
import Oath.Uri.Uri
import Oath.Uri.UriReference

-- | Section 5.2.2, Transform References
resolveUriReference ∷ AbsoluteUri → UriReference → Uri
resolveUriReference base r
  | Just scheme ← r ^? #scheme =
      Uri
        { scheme
        , hierPart = r.hierPart & #path %~ removeDotSegments
        , query = r.query
        , fragment = r.fragment
        }
  | Just authority ← r ^? #authority =
      Uri
        { scheme = base.scheme
        , hierPart = HierPart_Authority authority $ removeDotSegments $ r ^. #path
        , query = r.query
        , fragment = r.fragment
        }
  | HierPart_Relative Seq.Empty ← r.hierPart =
      Uri
        { scheme = base.scheme
        , hierPart = base.hierPart
        , query = r.query <|> base.query
        , fragment = r.fragment
        }
  | otherwise =
      Uri
        { scheme = base.scheme
        , hierPart = case base.hierPart of
            HierPart_Authority a p →
              HierPart_Authority a $
                removeDotSegments $
                  case r.hierPart of
                    HierPart_Authority _ p' → p'
                    HierPart_Absolute p' → p'
                    HierPart_Relative p' → p <> p'
            HierPart_Relative p →
              HierPart_Relative $
                removeDotSegments $
                  case r.hierPart of
                    HierPart_Authority _ p' → p'
                    HierPart_Absolute p' → p'
                    HierPart_Relative p' → p <> p'
        , query = r.query
        , fragment = r.fragment
        }
      where
        pathCat
          | r ^. #absolute = r

-- , authority = base.authority
-- , path = removeDotSegments $ base.path <> r.path

-- | <https://www.rfc-editor.org/rfc/rfc3986#section-5.2.3> sort of
instance Semigroup HierPart where
  HierPart_Authority a b <> HierPart_Authority _ r =
    HierPart_Authority a $ removeDotSegments (b <> r)

-- _ <> x@Path {base = PathAbsolute} = x
-- Path {base, segments = Empty} <> Path {segments} =
--   Path {base, segments}
-- Path {base, segments = b :|> _} <> Path {segments = r} =
--   Path {base, segments = b <> r}

instance Monoid HierPart where
  mempty = HierPart_Relative Seq.Empty

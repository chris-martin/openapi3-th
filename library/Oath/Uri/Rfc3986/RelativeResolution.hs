-- | <https://www.ietf.org/rfc/rfc3986.txt>
--   Section 5.2, Relative Resolution
module Oath.Uri.Rfc3986.RelativeResolution where

import Essentials

import Data.Text(Text)

import Oath.Uri.Rfc3986.Grammar
import Data.Sequence (Seq(..))

resolve ∷ Uri → UriReference → Uri
resolve b r = _

-- | Section 5.2.3, Merge Paths
-- mergePaths :: Uri -> UriReference -> _

-- | Section 5.2.4, Remove Dot Segments
removeDotSegments :: Seq Text -> Seq Text
removeDotSegments = go Empty
 where
  go t = \case
    Empty -> t
    Empty :|> ".." -> t
    xs :|> "." -> go t xs
    xs :|> _ :|> ".." -> go t xs
    xs :|> x -> go (x :<| t) xs

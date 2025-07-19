module Oath.Uri.Rfc3986.Path where

import Essentials

import Data.ByteString (ByteString)
import Data.Sequence (Seq (..))

data Path = Path
  { base ∷ PathBase
  , segments ∷ Seq ByteString
  }

data PathBase
  = PathRelative
  | PathAbsolute

-- | <https://www.rfc-editor.org/rfc/rfc3986#section-5.2.3> sort of
instance Semigroup Path where
  _ <> x@Path {base = PathAbsolute} = x
  Path {base, segments = Empty} <> Path {segments} =
    Path {base, segments}
  Path {base, segments = b :|> _} <> Path {segments = r} =
    Path {base, segments = b <> r}

instance Monoid Path where
  mempty = Path {base = PathRelative, segments = Empty}

-- | <https://www.rfc-editor.org/rfc/rfc3986#section-5.2.4>
removeDotSegments ∷ Path → Path
removeDotSegments p = p {segments = go Empty p.segments}
 where
  go t = \case
    Empty → t
    Empty :|> ".." → t
    xs :|> "." → go t xs
    xs :|> _ :|> ".." → go t xs
    xs :|> x → go (x :<| t) xs

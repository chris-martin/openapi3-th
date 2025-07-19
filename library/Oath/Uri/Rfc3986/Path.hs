module Oath.Uri.Rfc3986.Path where

import Essentials

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Either (Either (..))
import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text (Text)

import Oath.Uri.Rfc3986.Grammar (AbsoluteUri (..), Authority (..))
import Oath.Uri.Rfc3986.Grammar qualified as G

data Path = Path {root ∷ PathRoot, segments ∷ Seq ByteString}

data PathRoot = PathRelative | PathAbsolute

-- | <https://www.rfc-editor.org/rfc/rfc3986#section-5.2.3> sort of
instance Semigroup Path where
  _ <> x@Path {root = PathAbsolute} = x
  Path {root, segments = Empty} <> Path {segments} =
    Path {root, segments}
  Path {root, segments = base :|> _} <> Path {segments = r} =
    Path {root, segments = base <> r}

instance Monoid Path where
  mempty = Path PathRelative Empty

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

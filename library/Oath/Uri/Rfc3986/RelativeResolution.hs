-- | <https://www.ietf.org/rfc/rfc3986.txt>
--   Section 5.2, Relative Resolution
module Oath.Uri.Rfc3986.RelativeResolution (
  resolveUriReference,
  BaseUri (..),
  UriReference (..),
  Uri (..),
  Path (..),
) where

import Essentials

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text (Text)

import Oath.Uri.Rfc3986.Grammar (AbsoluteUri (..), Authority (..))
import Oath.Uri.Rfc3986.Grammar qualified as G

data BaseUri = BaseUri
  { scheme ∷ Text
  , authority ∷ Maybe Authority
  , path ∷ Path
  , query ∷ Maybe Text
  }

baseUriFromGrammar ∷ AbsoluteUri → BaseUri
baseUriFromGrammar x@AbsoluteUri {scheme, query} =
  BaseUri {scheme, query, authority, path}
 where
  (authority, path) = fromHierPart x.hierPart

data UriReference = UriReference
  { scheme ∷ Maybe Text
  , authority ∷ Maybe Authority
  , path ∷ Path
  , query ∷ Maybe Text
  , fragment ∷ Maybe Text
  }

uriReferenceFromGrammar ∷ G.UriReference → UriReference
uriReferenceFromGrammar = \case
  G.UriReference_Uri x@G.Uri {query, fragment} →
    UriReference {scheme, authority, path, query, fragment}
   where
    scheme = Just x.scheme
    (authority, path) = fromHierPart x.hierPart
  G.UriReference_RelativeRef x@G.RelativeRef {query, fragment} →
    UriReference {scheme, authority, path, query, fragment}
   where
    scheme = Nothing
    (authority, path) = fromRelativePart x.relativePart

data PathRoot = PathRelative | PathAbsolute

data Path = Path {root ∷ PathRoot, segments ∷ Seq Text}

data Uri = Uri
  { scheme ∷ Text
  , authority ∷ Maybe Authority
  , path ∷ Path
  , query ∷ Maybe Text
  , fragment ∷ Maybe Text
  }

fromHierPart ∷ G.HierPart → (Maybe Authority, Path)
fromHierPart = \case
  G.HierPart_Authority a p → (Just a, Path PathAbsolute p)
  G.HierPart_Absolute p → (Nothing, Path PathAbsolute p)
  G.HierPart_Rootless p → (Nothing, Path PathRelative $ NESeq.toSeq p)
  G.HierPart_Empty → (Nothing, Path PathRelative [])

fromRelativePart ∷ G.RelativePart → (Maybe Authority, Path)
fromRelativePart = \case
  G.RelativePart_Authority a p → (Just a, Path PathAbsolute p)
  G.RelativePart_Absolute p → (Nothing, Path PathAbsolute p)
  G.RelativePart_Noscheme p → (Nothing, Path PathRelative $ NESeq.toSeq p)
  G.RelativePart_Empty → (Nothing, Path PathRelative [])

-- | Section 5.2.2, Transform References
resolveUriReference ∷ BaseUri → UriReference → Uri
resolveUriReference base r
  | Just scheme ← r.scheme =
      Uri
        { scheme
        , authority = r.authority
        , path = removeDotSegments r.path
        , query = r.query
        , fragment = r.fragment
        }
  | Just authority ← r.authority =
      Uri
        { scheme = base.scheme
        , authority = Just authority
        , path = removeDotSegments r.path
        , query = r.query
        , fragment = r.fragment
        }
  | Path {root = PathRelative, segments = Empty} ← r.path =
      Uri
        { scheme = base.scheme
        , authority = base.authority
        , path = base.path
        , query = r.query <|> base.query
        , fragment = r.fragment
        }
  | otherwise =
      Uri
        { scheme = base.scheme
        , authority = base.authority
        , path = removeDotSegments $ base.path <> r.path
        , query = r.query
        , fragment = r.fragment
        }

-- | Section 5.2.3, Merge Paths, sort of
instance Semigroup Path where
  _ <> x@Path {root = PathAbsolute} = x
  Path {root, segments = Empty} <> Path {segments} =
    Path {root, segments}
  Path {root, segments = base :|> _} <> Path {segments = r} =
    Path {root, segments = base <> r}

instance Monoid Path where
  mempty = Path PathRelative Empty

-- | Section 5.2.4, Remove Dot Segments
removeDotSegments ∷ Path → Path
removeDotSegments p = p {segments = go Empty p.segments}
 where
  go t = \case
    Empty → t
    Empty :|> ".." → t
    xs :|> "." → go t xs
    xs :|> _ :|> ".." → go t xs
    xs :|> x → go (x :<| t) xs

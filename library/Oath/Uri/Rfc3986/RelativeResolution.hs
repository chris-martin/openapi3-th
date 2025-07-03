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

import Data.Foldable (toList)
import Data.Sequence (Seq (..))
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
  G.UriReference_RelativeRef x@G.RelativeRef{query,fragment} →
    UriReference{scheme, authority, path, query, fragment}
   where
    scheme=Nothing
    (authority,path) = fromRelativePart x.relativePart

data Path
  = PathRelative [Text]
  | PathAbsolute [Text]

data Uri = Uri
  { scheme ∷ Text
  , authority ∷ Maybe Authority
  , path ∷ Path
  , query ∷ Maybe Text
  , fragment ∷ Maybe Text
  }

fromHierPart ∷ G.HierPart → (Maybe Authority, Path)
fromHierPart = \case
  G.HierPart_Authority a p → (Just a, PathAbsolute p)
  G.HierPart_Absolute p → (Nothing, PathAbsolute p)
  G.HierPart_Rootless p → (Nothing, PathRelative $ toList p)
  G.HierPart_Empty → (Nothing, PathRelative [])

fromRelativePart :: G.RelativePart -> (Maybe Authority, Path)
fromRelativePart = \case
  G.RelativePart_Authority a p -> (Just a, PathAbsolute p)
  G.RelativePart_Absolute p -> (Nothing, PathAbsolute p)
  G.RelativePart_Noscheme p -> (Nothing, PathRelative $ toList p)
  G.RelativePart_Empty -> (Nothing, PathRelative [])

resolveUriReference ∷ BaseUri → UriReference → Uri
resolveUriReference b r = _

-- | Section 5.2.3, Merge Paths
-- mergePaths :: Uri -> UriReference -> _

-- | Section 5.2.4, Remove Dot Segments
removeDotSegments ∷ Seq Text → Seq Text
removeDotSegments = go Empty
 where
  go t = \case
    Empty → t
    Empty :|> ".." → t
    xs :|> "." → go t xs
    xs :|> _ :|> ".." → go t xs
    xs :|> x → go (x :<| t) xs

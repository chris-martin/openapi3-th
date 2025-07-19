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
import Data.ByteString (ByteString)
import Data.Either (Either (..))
import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text (Text)

import Oath.Uri.Rfc3986.Grammar (AbsoluteUri (..), Authority (..))
import Oath.Uri.Rfc3986.Grammar qualified as G
import Oath.Uri.Rfc3986.Path qualified as G

data Uri = Uri
  { scheme ∷ ByteString
  , authority ∷ Maybe Authority
  , path ∷ Path
  , query ∷ Maybe ByteString
  , fragment ∷ Maybe ByteString
  }

data BaseUri = BaseUri
  { scheme ∷ ByteString
  , authority ∷ Maybe Authority
  , path ∷ Path
  , query ∷ Maybe ByteString
  }

data UriReference = UriReference
  { scheme ∷ Maybe ByteString
  , authority ∷ Maybe Authority
  , path ∷ Path
  , query ∷ Maybe ByteString
  , fragment ∷ Maybe ByteString
  }

baseUriFromGrammar ∷ AbsoluteUri → BaseUri
baseUriFromGrammar x@AbsoluteUri {scheme, query} =
  BaseUri {scheme, query, authority, path}
 where
  (authority, path) = fromHierPart x.hierPart

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

uriToGrammar ∷ Uri → Either InvalidHierPart G.Uri
uriToGrammar x@Uri {scheme, query, fragment} = do
  hierPart ← toHierPart (x.authority, x.path)
  pure G.Uri {scheme, hierPart, query, fragment}

toHierPart
  ∷ (Maybe Authority, Path)
  → Either InvalidHierPart G.HierPart
toHierPart = \case
  (Just a, Path PathAbsolute p) → pure $ G.HierPart_Authority a p
  (Just {}, Path PathRelative _) → Left AuthorityWithRelativePath
  (Nothing, Path PathAbsolute p) → _
  (Nothing, Path PathRelative p) → _

data InvalidHierPart = AuthorityWithRelativePath

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

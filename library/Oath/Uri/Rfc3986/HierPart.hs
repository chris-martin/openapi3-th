module Oath.Uri.Rfc3986.HierPart (
  -- * Types
  HierPart (..),
  HierPartBase (..),

  -- * Projections
  hierPartBase,
  hierPartPathSegments,
  hierPartAuthority,
  hierPartPath,
  hierPartBaseAuthority,
  hierPartBasePathBase,

  -- * Grammar isomorphisms
  hierPartGrammarIso,
  hierPartRelativeGrammarIso,
) where

import Essentials

import Data.ByteString (ByteString)
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty qualified as NESeq
import Optics

import Oath.Uri.Rfc3986.Grammar (Authority (..))
import Oath.Uri.Rfc3986.Grammar qualified as G
import Oath.Uri.Rfc3986.Path

-- | Consisting of a path and an optional authority, 'HierPart' is a
--   constituent of URIs, URI references, and base URIs.
data HierPart = HierPart
  { base ∷ HierPartBase
  , pathSegments ∷ Seq ByteString
  }

data HierPartBase
  = HierPart_Absolute (Maybe Authority)
  | HierPart_Relative

makeFieldLabels ''HierPart
makePrismLabels ''HierPartBase

hierPartBase ∷ HierPart → HierPartBase
hierPartBase = (.base)

hierPartPathSegments ∷ HierPart → Seq ByteString
hierPartPathSegments = (.pathSegments)

hierPartAuthority ∷ HierPart → Maybe Authority
hierPartAuthority = hierPartBaseAuthority . hierPartBase

hierPartBaseAuthority ∷ HierPartBase → Maybe Authority
hierPartBaseAuthority = \case
  HierPart_Absolute x → x
  HierPart_Relative → Nothing

hierPartPath ∷ HierPart → Path
hierPartPath HierPart {base, pathSegments} =
  Path {base = hierPartBasePathBase base, segments = pathSegments}

hierPartBasePathBase ∷ HierPartBase → PathBase
hierPartBasePathBase = \case
  HierPart_Absolute {} → PathAbsolute
  HierPart_Relative → PathRelative

hierPartGrammarIso ∷ Iso' HierPart G.HierPart
hierPartGrammarIso =
  iso
    ( \HierPart {base, pathSegments} → case base of
        HierPart_Absolute (Just authority) →
          G.HierPart_Authority authority pathSegments
        HierPart_Absolute Nothing →
          G.HierPart_Absolute pathSegments
        HierPart_Relative →
          maybe G.HierPart_Empty G.HierPart_Rootless $
            NESeq.nonEmptySeq pathSegments
    )
    ( \case
        G.HierPart_Authority authority pathSegments →
          HierPart {base = HierPart_Absolute (Just authority), pathSegments}
        G.HierPart_Absolute pathSegments →
          HierPart {base = HierPart_Absolute Nothing, pathSegments}
        G.HierPart_Rootless pathSegments →
          HierPart {base = HierPart_Relative, pathSegments = NESeq.toSeq pathSegments}
        G.HierPart_Empty →
          HierPart {base = HierPart_Relative, pathSegments = []}
    )

hierPartRelativeGrammarIso ∷ Iso' HierPart G.RelativePart
hierPartRelativeGrammarIso =
  iso
    ( \HierPart {base, pathSegments} → case base of
        HierPart_Absolute (Just authority) →
          G.RelativePart_Authority authority pathSegments
        HierPart_Absolute Nothing →
          G.RelativePart_Absolute pathSegments
        HierPart_Relative →
          maybe G.RelativePart_Empty G.RelativePart_Noscheme $
            NESeq.nonEmptySeq pathSegments
    )
    ( \case
        G.RelativePart_Authority authority pathSegments →
          HierPart {base = HierPart_Absolute (Just authority), pathSegments}
        G.RelativePart_Absolute pathSegments →
          HierPart {base = HierPart_Absolute Nothing, pathSegments}
        G.RelativePart_Noscheme pathSegments →
          HierPart {base = HierPart_Relative, pathSegments = NESeq.toSeq pathSegments}
        G.RelativePart_Empty →
          HierPart {base = HierPart_Relative, pathSegments = []}
    )

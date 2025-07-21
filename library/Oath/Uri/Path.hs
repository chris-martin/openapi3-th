module Oath.Uri.Path where

import Essentials

import Data.ByteString (ByteString)
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty (NESeq (..))
import Optics hiding (Empty)

import Oath.Grammar
import Oath.Uri.Segment

-- | <https://www.rfc-editor.org/rfc/rfc3986#section-5.2.4>
removeDotSegments ∷ Seq ByteString → Seq ByteString
removeDotSegments = go Empty
 where
  go t = \case
    Empty → t
    Empty :|> ".." → t
    xs :|> "." → go t xs
    xs :|> _ :|> ".." → go t xs
    xs :|> x → go (x :<| t) xs

pathAbemptyGrammar ∷ Grammar (Seq ByteString)
pathAbemptyGrammar =
  label "path-abempty" $
    seqGrammar $
      constGrammar "/" +> segmentGrammar

pathAbsoluteGrammar ∷ Grammar (Seq ByteString)
pathAbsoluteGrammar =
  label "path-absolute"
    $ isoGrammar
      ( iso
          (\case Empty → Nothing; x :<| xs → Just (x :& xs))
          (\case Nothing → Empty; Just (x :& xs) → x :<| xs)
      )
    $ constGrammar "/"
      +> optionalGrammar
        (segmentNzGrammar <+> seqGrammar (constGrammar "/" +> segmentGrammar))

pathNoschemeGrammar ∷ Grammar (NESeq ByteString)
pathNoschemeGrammar =
  label "path-noscheme"
    $ isoGrammar
      ( iso
          (\(x :<|| xs) → x :& xs)
          (\(x :& xs) → x :<|| xs)
      )
    $ segmentNzNcGrammar
      <+> seqGrammar (constGrammar "/" +> segmentGrammar)

pathRootlessGrammar ∷ Grammar (NESeq ByteString)
pathRootlessGrammar =
  label "path-rootless"
    $ isoGrammar
      ( iso
          (\(x :<|| xs) → x :& xs)
          (\(x :& xs) → x :<|| xs)
      )
    $ segmentNzGrammar
      <+> seqGrammar (constGrammar "/" +> segmentGrammar)

pathEmptyGrammar ∷ Grammar ()
pathEmptyGrammar = emptyGrammar

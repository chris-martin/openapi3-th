module Oath.Uri.Rfc3986.Path where

import Essentials

import Control.Applicative (Alternative (..), asum, liftA2)
import Control.Monad (mfilter, replicateM, replicateM_, sequence, unless)
import Control.Monad.Fail
import Control.Monad.Validate
import Data.Bifunctor (first)
import Data.Bits (shiftL, shiftR, toIntegralSized, (.&.))
import Data.Bool (not, (&&), (||))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL
import Data.Char (Char)
import Data.Char qualified as Char
import Data.Either (Either (..), either)
import Data.Foldable (fold, foldMap, foldl')
import Data.Function (const)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq (..))
import Data.Sequence.NonEmpty qualified as NESeq
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TB
import Data.Tuple
import Data.Vector qualified as V
import Data.Word
import GHC.Generics
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Numeric.Natural (Natural)
import Optics hiding (Empty)
import Optics.TH
import Test.QuickCheck (Gen, liftArbitrary)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary.Generic
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Show (show)
import Prelude (fromIntegral, (*), (+), (-))

import Oath.Abnf.Rfc2234
import Oath.Grammar
import Oath.Uri.Rfc3986.Appendages
import Oath.Uri.Rfc3986.Authority
import Oath.Uri.Rfc3986.Characters
import Oath.Uri.Rfc3986.Host
import Oath.Uri.Rfc3986.Scheme
import Oath.Uri.Rfc3986.Segment

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
        ( segmentNzGrammar <+> seqGrammar (constGrammar "/" +> segmentGrammar)
        )

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

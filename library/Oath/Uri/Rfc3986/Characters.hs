module Oath.Uri.Rfc3986.Characters where

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

pcharGrammar ∷ Grammar Word8
pcharGrammar =
  label
    "pchar"
    Grammar
      { render =
          renderChoices
            [ render unreservedGrammar
            , render subDelimGrammar
            , render etc
            , render pctEncodedGrammar
            ]
      , generator = arbitrary
      , parser =
          asum @[]
            [ parser unreservedGrammar
            , parser subDelimGrammar
            , parser etc
            , parser pctEncodedGrammar
            ]
      }
 where
  etc = tokenEnumeration $ char <$> ":@"

pctEncodedGrammar ∷ Grammar Word8
pctEncodedGrammar =
  label
    "pct-encoded"
    Grammar
      { render = \x → do
          let pct = renderConst $ BSB.word8 $ char '%'
          a ← render (hexdigGrammar UpperCase) (x `shiftR` 4)
          b ← render (hexdigGrammar UpperCase) (x .&. 15)
          Just $ renderConcat [pct, a, b]
      , parser = do
          P.single $ char '%'
          a ← parser $ hexdigGrammar UpperCase
          b ← parser $ hexdigGrammar UpperCase
          pure $ (a `shiftL` 4) + b
      , generator = arbitrary
      }

unreservedGrammar ∷ Grammar Word8
unreservedGrammar =
  label
    "unreserved"
    Grammar
      { render =
          renderChoices
            [ render alphaGrammar
            , render digitGrammar
            , render etc
            ]
      , parser =
          asum @[]
            [ parser alphaGrammar
            , parser digitGrammar
            , parser etc
            ]
      , generator =
          QC.oneof
            [ generator alphaGrammar
            , generator digitGrammar
            , generator etc
            ]
      }
 where
  etc = tokenEnumeration $ char <$> "-._~"
  r = BSB.word8

reservedGrammar ∷ Grammar Word8
reservedGrammar = label "reserved" $ tokenEnumeration $ genDelims <> subDelims

genDelimGrammar ∷ Grammar Word8
genDelimGrammar = label "gen-delims" $ tokenEnumeration genDelims

genDelims ∷ [Word8]
genDelims = char <$> ":/?#[]@"

subDelimGrammar ∷ Grammar Word8
subDelimGrammar = label "sub-delims" $ tokenEnumeration subDelims

subDelims ∷ [Word8]
subDelims = char <$> "!$&'()*+,;="

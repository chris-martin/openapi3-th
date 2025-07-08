module Oath.Uri.Rfc3986.Segment where

import Essentials

import Control.Applicative (Alternative (..), asum, liftA2)
import Control.Monad (guard, mfilter, replicateM, replicateM_, sequence, unless)
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
import Oath.Uri.Rfc3986.Characters
import Oath.Uri.Rfc3986.Host
import Oath.Uri.Rfc3986.Scheme

segmentGrammar ∷ Grammar ByteString
segmentGrammar =
  label
    "segment"
    Grammar
      { render =
          fmap renderConcat . traverse (render pcharGrammar) . BS.unpack
      , parser =
          fmap BS.pack $ P.many $ parser pcharGrammar
      , generator =
          fmap (build . fold) $ QC.listOf $ renderGenerator pcharGrammar
      }

segmentNzGrammar ∷ Grammar ByteString
segmentNzGrammar =
  label
    "segment-nz"
    Grammar
      { render = \x → do
          guard $ not $ BS.null x
          fmap renderConcat $ traverse (render pcharGrammar) $ BS.unpack x
      , parser =
          fmap BS.pack $ P.some $ parser pcharGrammar
      , generator =
          fmap BS.pack $ QC.listOf1 $ generator pcharGrammar
      }

segmentNzNcGrammar ∷ Grammar ByteString
segmentNzNcGrammar =
  label
    "segment-nz-nc"
    Grammar
      { render = \x → do
          guard $ not $ BS.null x
          fmap renderConcat $ traverse (render charGrammar) $ BS.unpack x
      , parser = fmap BS.pack $ P.some $ parser charGrammar
      , generator = fmap BS.pack $ QC.listOf1 $ generator charGrammar
      }
 where
  charGrammar =
    Grammar
      { render =
          renderChoices
            [ render unreservedGrammar
            , render subDelimGrammar
            , render etc
            , render pctEncodedGrammar
            ]
      , parser =
          asum @[]
            [ unreservedGrammar.parser
            , subDelimGrammar.parser
            , etc.parser
            , pctEncodedGrammar.parser
            ]
      , generator =
          QC.oneof
            [ generator unreservedGrammar
            , generator subDelimGrammar
            , generator etc
            , generator pctEncodedGrammar
            ]
      }
  etc = tokenEnumeration $ char <$> "@"

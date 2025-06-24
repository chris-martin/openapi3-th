-- | https://www.rfc-editor.org/rfc/rfc2234
module OpenApiTH.Web.Rfc2234 where

import Essentials

import Control.Applicative (Alternative (..), asum, liftA2)
import Control.Monad (mfilter, replicateM, replicateM_, unless)
import Control.Monad.Fail
import Control.Monad.Validate
import Data.Bifunctor (first)
import Data.Bool (not, (&&), (||))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (Char)
import Data.Char qualified as Char
import Data.Either (Either (..), either)
import Data.Function (const)
import Data.List qualified as List
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Tuple
import Data.Vector qualified as V
import Data.Word
import GHC.Generics
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Numeric.Natural (Natural)
import Optics.TH
import Test.QuickCheck (Gen)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary.Generic
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Show (show)
import Prelude (fromIntegral)

import OpenApiTH.Grammar

newtype Alpha = AlphaUnsafe Char
 deriving Grammar via Named "ALPHA" (Tested Alpha)
 deriving IsChar via CoercedChar Alpha

instance Testable Alpha where
  charIs x =
    (x >= 'a' && x <= 'z')
      || (x >= 'A' && x <= 'Z')

instance Arbitrary Alpha where
  arbitrary =
    fmap AlphaUnsafe $
      QC.oneof
        [ QC.choose ('a', 'z')
        , QC.choose ('A', 'Z')
        ]

newtype Digit = DigitUnsafe Char
  deriving Grammar via Named "DIGIT" (Tested Digit)
  deriving IsChar via CoercedChar Digit

instance Testable Digit where
  charIs x =     x >= '0' && x <= '9'

instance Arbitrary Digit where
  arbitrary = fmap DigitUnsafe $ QC.choose ('0', '9')

newtype Hexdig = HexdigUnsafe Char
  deriving Grammar via Named "HEXDIG" (Tested Hexdig)
  deriving IsChar via CoercedChar Hexdig

instance Testable Hexdig where
  charIs x = charIs @Digit x || charIs @HexLetter x

instance Arbitrary Hexdig where
  arbitrary =
    fmap HexdigUnsafe $
      QC.frequency
        [ (10, (\(DigitUnsafe x) → x) <$> arbitrary)
        , (6, (\(HexLetterUnsafe x) → x) <$> arbitrary)
        ]

newtype HexLetter = HexLetterUnsafe Char
  deriving Grammar via Tested HexLetter
  deriving IsChar via CoercedChar HexLetter

instance Testable HexLetter where
  charIs x =
    (x >= 'A' && x <= 'F')
      || (x >= 'a' && x <= 'f')

instance Arbitrary HexLetter where
  arbitrary =
    fmap HexLetterUnsafe $
      QC.oneof
        [ QC.choose ('A', 'F')
        , QC.choose ('a', 'f')
        ]

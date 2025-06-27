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
import Prelude (Num ((+), (-)), fromIntegral)

import Data.Text.Lazy.Builder qualified as TB
import OpenApiTH.Grammar

newtype Alpha = AlphaUnsafe {char ∷ Char}
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

newtype DigitChar = DigitCharUnsafe {char ∷ Char}
  deriving Grammar via Named "DIGIT" (Tested DigitChar)
  deriving IsChar via CoercedChar DigitChar

instance Testable DigitChar where
  charIs x = x >= '0' && x <= '9'

instance Arbitrary DigitChar where
  arbitrary = fmap DigitCharUnsafe $ QC.choose ('0', '9')

newtype DigitNum = DigitNumUnsafe {byte ∷ Word8}

instance Grammar DigitNum where
  render x = TB.singleton $ Char.chr $ Char.ord '0' + fromIntegral x.byte
  parser =
    P.label "DIGIT"
      $ fmap
        ( \x →
            DigitNumUnsafe $ fromIntegral $ Char.ord x.char - Char.ord '0'
        )
      $ parser @DigitChar

instance Arbitrary DigitNum where
  arbitrary = fmap DigitNumUnsafe $ QC.choose (0, 9)

data Case = UpperCase | LowerCase

class IsCase (c ∷ Case) where
  caseA ∷ Char

instance IsCase UpperCase where
  caseA = 'A'

instance IsCase LowerCase where
  caseA = 'a'

newtype Hexdig (c ∷ Case) = HexdigUnsafe {byte ∷ Word8}

hexdigChar ∷ ∀ c. IsCase c ⇒ Hexdig c → Char
hexdigChar x =
  Char.chr $
    Char.ord (if x.byte < 10 then '0' else caseA @c) + fromIntegral x.byte

-- | @c@ is the case for used for rendering.
--   Parsing accepts either.
instance IsCase c ⇒ Grammar (Hexdig c) where
  render = TB.singleton . hexdigChar
  parser =
    P.label "HEXDIG" $
      fmap HexdigUnsafe $
        asum @[]
          [ (.byte) <$> parser @DigitNum
          , (.byte) <$> parser @(HexLetter c)
          ]

-- | @c@ is ignored.
instance Testable (Hexdig c) where
  charIs x = charIs @DigitChar x || charIs @(HexLetter c) x

-- | @c@ is ignored.
instance Arbitrary (Hexdig c) where
  arbitrary =
    fmap HexdigUnsafe $
      QC.frequency
        [ (10, (.byte) <$> arbitrary @DigitNum)
        , (6, (.byte) <$> arbitrary @(HexLetter c))
        ]

newtype HexLetter (c ∷ Case) = HexLetterUnsafe {byte ∷ Word8}

-- | @c@ is the case for used for rendering.
--   Parsing accepts either.
instance IsCase c ⇒ Grammar (HexLetter c) where
  render x =
    TB.singleton $
      Char.chr $
        Char.ord (caseA @c) + fromIntegral x.byte
  parser =
    fmap HexLetterUnsafe $
      asum @[]
        [ z 'A' 'F'
        , z 'a' 'f'
        ]
   where
    z a f = fmap
      ( \x →
          fromIntegral $
            Char.ord x - Char.ord a + 10
      )
      $ P.satisfy
      $ \x → x >= a && x <= f

-- | @c@ is ignored.
instance Testable (HexLetter c) where
  charIs x =
    (x >= 'A' && x <= 'F')
      || (x >= 'a' && x <= 'f')

-- | @c@ is ignored.
instance Arbitrary (HexLetter c) where
  arbitrary =
    fmap HexLetterUnsafe $
      QC.choose (10, 15)

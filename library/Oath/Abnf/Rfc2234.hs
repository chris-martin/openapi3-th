-- | https://www.rfc-editor.org/rfc/rfc2234
module Oath.Abnf.Rfc2234 (
  char,
  alphaGrammar,
  digitGrammar,
  digitNumGrammar,
  Case (..),
  hexdigGrammar,
  hexdigNumGrammar,
  hexdigCaseGrammar,
) where

import Essentials

import Control.Applicative (Alternative (..), asum, liftA2)
import Control.Monad (mfilter, replicateM, replicateM_, unless)
import Control.Monad.Fail
import Control.Monad.Validate
import Data.Bifunctor (first)
import Data.Bool (not, (&&), (||))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
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
import Data.Text.Lazy.Builder qualified as TB
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
import Text.Megaparsec.Byte.Lexer qualified as P
import Text.Show (show)
import Prelude (Num ((+), (-)), fromIntegral)

import Oath.Grammar

char ∷ Char → Word8
char = fromIntegral . Char.ord

alphaGrammar ∷ Grammar Word8
alphaGrammar =
  label "ALPHA" $
    tokenPredicate
      ( \x →
          (x >= char 'a' && x <= char 'z')
            || (x >= char 'A' && x <= char 'Z')
      )
      ( QC.oneof
          [ QC.choose (char 'a', char 'z')
          , QC.choose (char 'A', char 'Z')
          ]
      )

digitGrammar ∷ Grammar Word8
digitGrammar =
  label "DIGIT" $
    tokenPredicate
      (\x → x >= char '0' && x <= char '0')
      (QC.choose (char '0', char '0'))

digitNumGrammar ∷ Grammar Word8
digitNumGrammar =
  Grammar
    { render = \x → BSB.char8 $ Char.chr $ Char.ord '0' + fromIntegral x
    , parser = (\x → x - char '0') <$> digitGrammar.parser
    , generator = QC.choose (0, 9)
    }

data Case = UpperCase | LowerCase
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving Arbitrary via GenericArbitrary Case

caseA ∷ Case → Word8
caseA = fst . caseAF

caseAF ∷ Case → (Word8, Word8)
caseAF = \case
  UpperCase → (char 'A', char 'F')
  LowerCase → (char 'a', char 'f')

isHexLetterInCase ∷ Case → Word8 → Bool
isHexLetterInCase c x = let (a, f) = caseAF c in x >= a && x <= f

hexdigToChar ∷ Case → Word8 → Word8
hexdigToChar c x =
  x + (if x < 10 then char '0' else caseA c)

hexLetterNumParserInCase ∷ Case → Parsec Void ByteString Word8
hexLetterNumParserInCase c =
  let (a, f) = caseAF c
   in fmap
        (\x → x - a + 10)
        $ P.satisfy (\x → x >= a && x <= f)

hexdigGrammar ∷ Grammar Word8
hexdigGrammar =
  label
    "HEXDIG"
    Grammar
      { render = BSB.word8
      , parser = digitGrammar.parser <|> hexLetterGrammar.parser
      , generator = hexdigToChar <$> arbitrary <*> QC.choose (0, 15)
      }

hexdigNumGrammar
  ∷ Case
  -- ^ For rendering and generating; does not affect parsing
  → Grammar Word8
hexdigNumGrammar c =
  label
    "HEXDIG"
    Grammar
      { render = BSB.word8 . hexdigToChar c
      , parser =
          digitNumGrammar.parser
            <|> (hexLetterNumGrammar c).parser
      , generator = QC.choose (0, 15)
      }

hexdigCaseGrammar
  ∷ Case
  -- ^ For rendering and generating; parsing is lenient but is converted to this case
  → Grammar Word8
hexdigCaseGrammar c =
  label
    "HEXDIG"
    Grammar
      { render = BSB.word8
      , parser = digitGrammar.parser <|> (hexLetterCaseGrammar c).parser
      , generator = hexdigToChar c <$> QC.choose (0, 15)
      }

hexLetterGrammar ∷ Grammar Word8
hexLetterGrammar =
  tokenPredicate
    (\x → isHexLetterInCase LowerCase x || isHexLetterInCase UpperCase x)
    (QC.oneof $ QC.choose . caseAF <$> [LowerCase, UpperCase])

hexLetterCaseGrammar
  ∷ Case
  -- ^ For rendering and generating; parsing is lenient but is converted to this case
  → Grammar Word8
hexLetterCaseGrammar c =
  Grammar
    { render = BSB.word8
    , parser =
        asum @[]
          [ P.satisfy $ isHexLetterInCase c
          , case c of
              LowerCase →
                fmap (\x → x - char 'A' + char 'a') $
                  P.satisfy (isHexLetterInCase UpperCase)
              UpperCase →
                fmap (\x → x - char 'a' + char 'A') $
                  P.satisfy (isHexLetterInCase LowerCase)
          ]
    , generator = QC.choose $ caseAF c
    }

hexLetterNumGrammar
  ∷ Case
  -- ^ For rendering and generating; does not affect parsing
  → Grammar Word8
hexLetterNumGrammar c =
  Grammar
    { render = \x → BSB.word8 $ caseA c + x
    , parser =
        hexLetterNumParserInCase LowerCase
          <|> hexLetterNumParserInCase UpperCase
    , generator = QC.choose (10, 15)
    }

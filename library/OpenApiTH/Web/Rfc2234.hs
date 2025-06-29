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
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Show (show)
import Prelude (Num ((+), (-)), fromIntegral)

import OpenApiTH.Grammar

alphaGrammar ∷ Grammar Char
alphaGrammar =
  label "ALPHA" $
    tokenPredicate
      (\x → (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z'))
      (QC.oneof [QC.choose ('a', 'z'), QC.choose ('A', 'Z')])

digitGrammar ∷ Grammar Char
digitGrammar =
  label "DIGIT" $
    tokenPredicate
      (\x → x >= '0' && x <= '9')
      (QC.choose ('0', '9'))

digitNumGrammar ∷ Grammar Word8
digitNumGrammar =
  Grammar
    { render = \x → TB.singleton $ Char.chr $ Char.ord '0' + fromIntegral x
    , parser = (\x → fromIntegral $ Char.ord x - Char.ord '0') <$> digitGrammar.parser
    , generator = QC.choose (0, 9)
    }

data Case = UpperCase | LowerCase
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving Arbitrary via GenericArbitrary Case

caseA ∷ Case → Char
caseA = fst . caseAF

caseAF ∷ Case → (Char, Char)
caseAF = \case
  UpperCase → ('A', 'F')
  LowerCase → ('a', 'f')

isHexLetterInCase ∷ Case → Char → Bool
isHexLetterInCase c x = let (a, f) = caseAF c in x >= a && x <= f

hexdigToChar ∷ Case → Word8 → Char
hexdigToChar c x =
  Char.chr $
    Char.ord (if x < 10 then '0' else caseA c) + fromIntegral x

hexLetterNumParserInCase ∷ Case → Parsec Void Text Word8
hexLetterNumParserInCase c =
  let (a, f) = caseAF c
   in fmap
        (\x → fromIntegral $ Char.ord x - Char.ord a + 10)
        $ P.satisfy (\x → x >= a && x <= f)

hexdigGrammar ∷ Grammar Char
hexdigGrammar =
  label
    "HEXDIG"
    Grammar
      { render = TB.singleton
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
      { render = TB.singleton . hexdigToChar c
      , parser = digitNumGrammar.parser <|> (hexLetterNumGrammar c).parser
      , generator = QC.choose (0, 15)
      }

hexdigCaseGrammar
  ∷ Case
  -- ^ For rendering and generating; parsing is lenient but is converted to this case
  → Grammar Char
hexdigCaseGrammar c =
  label
    "HEXDIG"
    Grammar
      { render = TB.singleton
      , parser = digitGrammar.parser <|> (hexLetterCaseGrammar c).parser
      , generator = hexdigToChar c <$> QC.choose (0, 15)
      }

hexLetterGrammar ∷ Grammar Char
hexLetterGrammar =
  tokenPredicate
    (\x → isHexLetterInCase LowerCase x || isHexLetterInCase UpperCase x)
    (QC.oneof $ QC.choose . caseAF <$> [LowerCase, UpperCase])

hexLetterCaseGrammar
  ∷ Case
  -- ^ For rendering and generating; parsing is lenient but is converted to this case
  → Grammar Char
hexLetterCaseGrammar c =
  Grammar
    { render = TB.singleton
    , parser =
        asum @[]
          [ P.satisfy $ isHexLetterInCase c
          , case c of
              LowerCase →
                fmap (\x → Char.chr $ Char.ord x - Char.ord 'A' + Char.ord 'a') $
                  P.satisfy (isHexLetterInCase UpperCase)
              UpperCase →
                fmap (\x → Char.chr $ Char.ord x - Char.ord 'a' + Char.ord 'A') $
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
    { render = \x →
        TB.singleton $
          Char.chr $
            Char.ord (caseA c) + fromIntegral x
    , parser =
        hexLetterNumParserInCase LowerCase
          <|> hexLetterNumParserInCase UpperCase
    , generator = QC.choose (10, 15)
    }

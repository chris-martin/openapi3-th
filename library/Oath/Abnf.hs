-- | <https://www.rfc-editor.org/rfc/rfc2234>
module Oath.Abnf (
  char,
  alphaGrammar,
  digitNumGrammar,
  digitCharGrammar,
  Case (..),
  hexdigNumGrammar,
  hexdigCharGrammar,
) where

import Essentials

import Control.Monad (guard)
import Data.Bool ((&&), (||))
import Data.ByteString.Builder qualified as BSB
import Data.Char (Char)
import Data.Char qualified as Char
import Data.Tuple
import Data.Word
import GHC.Generics
import Optics
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary.Generic
import Text.Megaparsec qualified as P
import Prelude (Num ((+), (-)), fromIntegral)

import Oath.Grammar
import Oath.Grammar qualified as Grammar (Grammar (..))

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

digitNumGrammar ∷ Grammar Word8
digitNumGrammar =
  prismGrammar
    ( prism'
        (\x → x - char '0')
        ( \x → do
            guard $ x <= 9
            pure $ x + char '0'
        )
    )
    digitCharGrammar

digitCharGrammar ∷ Grammar Word8
digitCharGrammar =
  label
    "DIGIT"
    Grammar
      { render = Just . renderConst . BSB.word8
      , parser = P.satisfy (\x → x >= char '0' && x <= char '0')
      , generator = QC.choose (char '0', char '9')
      }

data Case = UpperCase | LowerCase
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving Arbitrary via GenericArbitrary Case

otherCase ∷ Case → Case
otherCase = \case
  UpperCase → LowerCase
  LowerCase → UpperCase

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

hexdigNumGrammar ∷ Case → Grammar Word8
hexdigNumGrammar c =
  (grammarAlternatives [digitNumGrammar, afNumGrammar c])
    { Grammar.generator = QC.choose (0, 15)
    }

hexdigCharGrammar ∷ Case → Grammar Word8
hexdigCharGrammar c =
  label "HEXDIG" $
    grammarAlternatives
      [ digitNumGrammar
      , afCharGrammar c
      ]

afNumGrammar ∷ Case → Grammar Word8
afNumGrammar c =
  prismGrammar
    ( prism'
        ( \x →
            if
              | x >= char 'a' && x <= char 'f' → x - char 'a'
              | x >= char 'A' && x <= char 'F' → x - char 'A'
              | otherwise → undefined
        )
        ( \x → do
            guard $ x <= 15
            pure $ x + caseA c
        )
    )
    (afCharGrammar c)

afCharGrammar ∷ Case → Grammar Word8
afCharGrammar c =
  Grammar
    { render = \x → do
        n ←
          if
            | x >= char 'a' && x <= char 'f' → Just $ x - char 'a'
            | x >= char 'A' && x <= char 'F' → Just $ x - char 'A'
            | otherwise → Nothing
        pure
          Render
            { canonical = BSB.word8 $ hexdigToChar c n
            , generator = do
                c' ← QC.elements [c, otherCase c]
                pure $ BSB.word8 $ hexdigToChar c' n
            }
    , parser = P.satisfy \x →
        (x >= char 'a' && x <= char 'f')
          || (x >= char 'A' && x <= char 'F')
    , generator =
        QC.oneof
          [ QC.choose (char 'a', char 'f')
          , QC.choose (char 'A', char 'F')
          ]
    }

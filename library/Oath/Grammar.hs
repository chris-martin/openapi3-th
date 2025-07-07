module Oath.Grammar where

import Essentials

import Control.Applicative (Alternative (..), asum, liftA2)
import Control.Monad (mfilter, replicateM, replicateM_, sequence, unless)
import Control.Monad.Fail
import Control.Monad.Validate
import Data.Bifunctor (first)
import Data.Bool (not, (&&), (||))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL
import Data.Char (Char)
import Data.Char qualified as Char
import Data.Coerce
import Data.Either (Either (..), either)
import Data.Foldable (fold, toList)
import Data.Function (const)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Maybe (mapMaybe)
import Data.Proxy
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Tuple
import Data.Vector qualified as V
import Data.Word
import GHC.Generics
import GHC.TypeLits
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
import Prelude (String, fromIntegral)

data Grammar a
  = Grammar
  { parser ∷ Parsec Void ByteString a
  , generator ∷ Gen a
  , render ∷ a → Maybe Render
  }

data Render = Render
  { canonical ∷ Builder
  , generator ∷ Gen Builder
  }

renderConst ∷ Builder → Render
renderConst canonical =
  Render {canonical, generator = pure canonical}

renderChoices ∷ [a → Maybe Render] → a → Maybe Render
renderChoices xs x = do
  os@(o :| _) ← nonEmpty $ mapMaybe ($ x) xs
  pure
    Render
      { canonical = o.canonical
      , generator = QC.oneof $ fmap (.generator) $ toList os
      }

renderConcat ∷ [Maybe Render] → Maybe Render
renderConcat =
  fmap
    ( \xs →
        Render
          { canonical = fold $ fmap (.canonical) xs
          , generator = fmap fold $ sequence $ fmap (.generator) xs
          }
    )
    . sequence

class HasGrammar a where
  grammar ∷ Grammar a

newtype TheGrammar a = TheGrammar a

instance HasGrammar a ⇒ Arbitrary (TheGrammar a) where
  arbitrary = TheGrammar <$> grammar.generator

render ∷ Grammar a → a → Maybe Render
render = (.render)

parser ∷ Grammar a → Parsec Void ByteString a
parser = (.parser)

generator ∷ Grammar a → Gen a
generator = (.generator)

label ∷ String → Grammar a → Grammar a
label l g = g {parser = P.label l g.parser}

tokenEnumeration ∷ [Word8] → Grammar Word8
tokenEnumeration xs =
  Grammar
    { parser = P.satisfy (`List.elem` xs)
    , generator = QC.elements xs
    , render = renderSimple BSB.word8
    }
 where
  r = BSB.word8

tokenPredicate ∷ (Word8 → Bool) → Gen Word8 → Grammar Word8
tokenPredicate f generator =
  Grammar
    { parser = P.satisfy f
    , generator
    , render = renderSimple BSB.word8
    }

renderSimple ∷ (a → Builder) → a → Maybe Render
renderSimple r x = Just $ renderConst $ r x

build ∷ Builder → BS.StrictByteString
build = BSL.toStrict . BSB.toLazyByteString

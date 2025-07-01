module OpenApiTH.Grammar where

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
import Data.Coerce
import Data.Either (Either (..), either)
import Data.Function (const)
import Data.List qualified as List
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
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Show (show)
import Prelude (String, fromIntegral)

data Grammar a
  = Grammar
  { parser ∷ Parsec Void Text a
  , render ∷ a → TB.Builder
  , generator ∷ Gen a
  }

class HasGrammar a where
  grammar ∷ Grammar a

newtype TheGrammar a = TheGrammar a

instance HasGrammar a ⇒ Arbitrary (TheGrammar a) where
  arbitrary = TheGrammar <$> grammar.generator

render ∷ Grammar a → a → TB.Builder
render = (.render)

parser ∷ Grammar a → Parsec Void Text a
parser = (.parser)

generator ∷ Grammar a → Gen a
generator = (.generator)

renderGenerator ∷ Grammar a → Gen TB.Builder
renderGenerator g = g.render <$> g.generator

label ∷ String → Grammar a → Grammar a
label l g = g {parser = P.label l g.parser}

tokenEnumeration ∷ [Char] → Grammar Char
tokenEnumeration xs =
  Grammar
    { parser = P.satisfy (`List.elem` xs)
    , render = TB.singleton
    , generator = QC.elements xs
    }

tokenPredicate ∷ (Char → Bool) → Gen Char → Grammar Char
tokenPredicate f generator =
  Grammar
    { parser = P.satisfy f
    , render = TB.singleton
    , generator
    }

textGrammar ∷ Parsec Void Text () → Gen TB.Builder → Grammar Text
textGrammar p g =
  Grammar
    { parser = fmap fst $ P.match p
    , generator = fmap (TL.toStrict . TB.toLazyText) g
    , render = TB.fromText
    }

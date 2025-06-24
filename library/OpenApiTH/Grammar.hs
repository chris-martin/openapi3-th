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
import Prelude (fromIntegral)
import Data.Coerce

class Grammar a where
  parser ∷ Parsec Void Text a

class IsChar a where
  fromCharUnsafe ∷ P.Token Text → a

newtype CoercedChar a = CoercedChar a

instance Coercible Char a => IsChar (CoercedChar a) where
  fromCharUnsafe = coerce

class Enumerable a where
  enumerate ∷ [P.Token Text]

newtype Enumerated a = Enumerated a

instance (Enumerable a,IsChar a) ⇒ Arbitrary (Enumerated a) where
  arbitrary = fmap (Enumerated . fromCharUnsafe) $ QC.elements $ enumerate @a

instance (Enumerable a,IsChar a) ⇒ Grammar (Enumerated a) where
  parser = fmap (Enumerated . fromCharUnsafe @a) $ P.satisfy \x → List.elem x $ enumerate @a

class Testable a where
  charIs :: P.Token Text -> Bool

newtype Tested a = Tested a

instance (Testable a,IsChar a) => Grammar (Tested a) where
  parser = fmap (Tested . fromCharUnsafe ) $ P.satisfy $ charIs @a

newtype Named (s ∷ Symbol) a = Named a

instance (KnownSymbol s, Grammar a) ⇒ Grammar (Named s a) where
  parser = fmap (Named @s) $ P.label (symbolVal $ Proxy @s) $ parser @a

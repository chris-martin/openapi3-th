module OpenApiTH.Web.Path where

import Essentials

import Control.Applicative (Alternative (..), asum)
import Control.Monad (mfilter, unless)
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
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Show (show)
import Prelude (fromIntegral)

pathP ∷ P.Parsec Void Text (Seq Text)
pathP =
  fmap (Seq.fromList . List.filter (not . Text.null)) $
    P.takeWhileP
      (Just "path character")
      (\x → not $ List.elem @[] x "/?#")
      `P.sepBy` P.single '/'

pathG ∷ Gen (Seq Text)
pathG =
  fmap Seq.fromList $
    QC.listOf $
      fmap Text.pack $
        QC.listOf1 $
          QC.oneof [QC.arbitraryPrintableChar, QC.arbitraryUnicodeChar]
            `QC.suchThat` (\x → Char.isPrint x && not (List.elem @[] x " /?#"))

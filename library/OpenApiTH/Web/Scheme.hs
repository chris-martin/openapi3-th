module OpenApiTH.Web.Scheme where

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

schemeP ∷ P.Parsec Void Text Text
schemeP = fmap fst $ P.match $ schemeHeadP *> schemeTailP

schemeG ∷ Gen Text
schemeG = Text.cons <$> schemeHeadG <*> schemeTailG

schemeHeadP ∷ P.Parsec Void Text Char
schemeHeadP = P.satisfy \x → Char.isAsciiLower x || Char.isAsciiUpper x

schemeHeadG ∷ Gen Char
schemeHeadG = QC.oneof [QC.choose ('a', 'z'), QC.choose ('A', 'Z')]

schemeTailP ∷ P.Parsec Void Text Text
schemeTailP = P.takeWhileP (Just "scheme character") \x →
  Char.isAsciiLower x
    || Char.isAsciiUpper x
    || Char.isDigit x
    || List.elem @[] x "+-."

schemeTailG ∷ Gen Text
schemeTailG =
  fmap Text.pack $
    QC.listOf $
      QC.oneof
        [ QC.choose ('a', 'z')
        , QC.choose ('A', 'Z')
        , QC.choose ('0', '9')
        , QC.elements "+-."
        ]

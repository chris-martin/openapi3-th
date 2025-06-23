module OpenApiTH.Web.RelativeReference where

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

import OpenApiTH.Web.Authority
import OpenApiTH.Web.Host
import OpenApiTH.Web.Path
import OpenApiTH.Web.ResourceContext
import OpenApiTH.Web.Scheme
import OpenApiTH.Web.UserInfo

data RelativeReference
  = RelativeReference
  { relativePart ∷ RelativePart
  , query ∷ Maybe Text
  , fragment ∷ Maybe Text
  }

data RelativePart
  = RelativePartAuthority Authority

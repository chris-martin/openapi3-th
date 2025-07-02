-- | Lax implementation of <https://www.rfc-editor.org/rfc/rfc3986>
module Oath.Web.ResourceLocation (
  ResourceLocation (..),
  ResourceContext (..),
  Authority (..),
  readResourceLocation,
  resourceLocationQQ,
  localhostPort,
  hostAuthority,
) where

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

import Oath.Web.Authority
import Oath.Web.Host
import Oath.Web.Path
import Oath.Web.ResourceContext
import Oath.Web.Scheme
import Oath.Web.UserInfo

-- | todo: Remodel this as "UriReference without a query or fragment"
data ResourceLocation = ResourceLocation
  { scheme ∷ Maybe Text
  , context ∷ ResourceContext
  , path ∷ Seq Text
  }
  deriving stock (Eq, Show, Lift, Generic)

instance Arbitrary ResourceLocation where
  arbitrary = resourceLocationG

-- | <https://datatracker.ietf.org/doc/html/rfc3986#section-5.2.2>
instance Semigroup ResourceLocation where
  b <> r
    | Just {} ← r.scheme = r
    | AuthorityContext {} ← r.context = r {scheme = b.scheme}
    | RelativeContext ← b.context
    , AbsoluteContext ← r.context =
        b {path = r.path, context = r.context}
    | AbsoluteContext ← r.context = b {path = r.path}
    | RelativeContext ← r.context = b {path = b.path <> r.path}

instance Monoid ResourceLocation where
  mempty = ResourceLocation {scheme = Nothing, context = RelativeContext, path = Empty}

readResourceLocation ∷ Text → Either [Text] ResourceLocation
readResourceLocation x =
  first ((: []) . Text.pack . show) $
    P.parse (resourceLocationP <* P.eof) "" x

resourceLocationP ∷ P.Parsec Void Text ResourceLocation
resourceLocationP = do
  scheme ← P.optional $ schemeP <* P.single ':'
  context ← contextP
  path ← pathP
  pure ResourceLocation {scheme, context, path}

resourceLocationG ∷ Gen ResourceLocation
resourceLocationG = do
  scheme ← QC.liftArbitrary schemeG
  context ← arbitrary
  path ← pathG
  pure ResourceLocation {scheme, context, path}

resourceLocationQQ ∷ QuasiQuoter
resourceLocationQQ =
  QuasiQuoter
    { quoteExp =
        either (fail . Text.unpack . Text.intercalate "\n") lift
          . readResourceLocation
          . Text.pack
    }

localhostPort ∷ Word16 → ResourceLocation
localhostPort port =
  ResourceLocation
    { scheme = Just "http"
    , context =
        AuthorityContext
          Authority
            { userInfo = Nothing
            , host = "localhost"
            , port = Just $ fromIntegral port
            }
    , path = Empty
    }

makeFieldLabelsNoPrefix ''ResourceLocation

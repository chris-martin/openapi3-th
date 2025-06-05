{-# OPTIONS_GHC -Wno-missing-fields #-}

module OpenApiTH.OpenApi.ResourceLocation where

import Essentials

import Control.Applicative (Alternative (..), asum)
import Control.Monad (mfilter, unless)
import Control.Monad.Fail
import Control.Monad.Validate
import Data.Bool (not, (||))
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
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Megaparsec qualified as P

data ResourceLocation = ResourceLocation
  { scheme ∷ Maybe Text
  , context ∷ ResourceContext
  , path ∷ Seq Text
  }
  deriving stock (Eq, Show, Lift)

data ResourceContext
  = AuthorityContext Authority
  | AbsoluteContext
  | RelativeContext
  deriving stock (Eq, Show, Lift)

data Authority = Authority
  { userInfo ∷ Maybe UserInfo
  , host ∷ Text
  , port ∷ Maybe Word16
  }
  deriving stock (Eq, Show, Lift)

data UserInfo = UserInfo {user ∷ Text, password ∷ Maybe Text}
  deriving stock (Eq, Show, Lift)

-- | https://datatracker.ietf.org/doc/html/rfc3986#section-5.2.2
instance Semigroup ResourceLocation where
  b <> r
    | Just {} ← r.scheme = r
    | AuthorityContext {} ← r.context =
        ResourceLocation {scheme = b.scheme, context = r.context, path = r.path}
    | Empty ← r.path = b
    | AbsoluteContext ← r.context = b {path = r.path}
    | RelativeContext ← r.context = b {path = b.path <> r.path}

-- todo: test for lawfulness
instance Monoid ResourceLocation where
  mempty = ResourceLocation {scheme = Nothing, context = RelativeContext, path = Empty}

readResourceLocation ∷ Text → Either [Text] ResourceLocation
readResourceLocation = _

-- readResourceLocation t = runValidate do
--   iri ← either (const $ refute ["Invalid IRI (RFC 3987)"]) pure $ P.iri t
--   let Iri (Iri.Scheme scheme) hierarchy (Iri.Query query) (Iri.Fragment fragment) = iri
--   let (context, path) =
--         case hierarchy of
--           Iri.AuthorisedHierarchy au p → (AuthorityContext $ makeAuthority au, makePath p)
--           Iri.AbsoluteHierarchy p → (AbsoluteContext, makePath p)
--           Iri.RelativeHierarchy p → (RelativeContext, makePath p)
--        where
--         makePath (Iri.Path p) = Seq.fromList $ fmap makePathSegment $ V.toList p
--         makePathSegment (Iri.PathSegment x) = x
--   unless (BS.null query) $ dispute ["Query must be empty"]
--   unless (BS.null fragment) $ dispute ["Fragment must be empty"]
--   pure ResourceLocation {scheme = mfilter (not . BS.null) $ Just scheme, context, path}

type Parser = P.Parsec Void Text

resourceLocationP ∷ Parser ResourceLocation
resourceLocationP = do
  scheme ← P.optional $ schemeP <* P.single ':'
  context ←
    asum @[] @Parser
      [ P.chunk "//" *> do
          AuthorityContext <$> do
            authorityP <* P.lookAhead (void (P.single '/') <|> P.eof)
      , P.single '/' $> AbsoluteContext
      , pure RelativeContext
      ]
  path ←
    P.takeWhileP
      (Just "path character")
      (\x → not $ List.elem @[] x "/?#")
      `P.sepBy` P.single '/'
  pure ResourceLocation {scheme, context}

schemeP ∷ Parser Text
schemeP =
  Text.cons
    <$> P.satisfy (\x → Char.isAsciiLower x || Char.isAsciiUpper x)
    <*> P.takeWhileP
      (Just "scheme character")
      ( \x →
          Char.isAsciiLower x
            || Char.isAsciiUpper x
            || Char.isDigit x
            || List.elem @[] x "+-."
      )

authorityP ∷ Parser Authority
authorityP = _

serverUrlQQ ∷ QuasiQuoter
serverUrlQQ =
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
          Authority {userInfo = Nothing, host = "localhost", port = Just port}
    , path = Empty
    }

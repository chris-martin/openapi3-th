{-# OPTIONS_GHC -Wno-missing-fields #-}

-- | Lax implementation of <https://www.rfc-editor.org/rfc/rfc3986>
module OpenApiTH.Web.ResourceLocation (
  ResourceLocation (..),
  ResourceContext (..),
  Authority (..),
  readResourceLocation,
  resourceLocationQQ,
  localhostPort,
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
import Test.QuickCheck (Gen)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary.Generic
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Show (show)
import Prelude (fromIntegral)

data ResourceLocation = ResourceLocation
  { scheme ∷ Maybe Text
  , context ∷ ResourceContext
  , path ∷ Seq Text
  }
  deriving stock (Eq, Show, Lift, Generic)

instance Arbitrary ResourceLocation where
  arbitrary = resourceLocationG

data ResourceContext
  = AuthorityContext Authority
  | AbsoluteContext
  | RelativeContext
  deriving stock (Eq, Show, Lift, Generic)
  deriving Arbitrary via GenericArbitrary ResourceContext

data Authority = Authority
  { userInfo ∷ Maybe Text
  , host ∷ Text
  , port ∷ Maybe Natural
  }
  deriving stock (Eq, Show, Lift, Generic)

instance Arbitrary Authority where
  arbitrary = authorityG

-- | <https://datatracker.ietf.org/doc/html/rfc3986#section-5.2.2>
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
readResourceLocation x =
  first ((: []) . Text.pack . show) $
    P.parse (resourceLocationP <* P.eof) "" x

type Parser = P.Parsec Void Text

resourceLocationP ∷ Parser ResourceLocation
resourceLocationP = do
  scheme ← P.optional $ schemeP <* P.single ':'
  context ← contextP
  path ← pathP
  pure ResourceLocation {scheme, context, path}

contextP ∷ Parser ResourceContext
contextP =
  asum @[] @Parser
    [ P.chunk "//" *> do
        AuthorityContext <$> do
          authorityP <* P.lookAhead (void (P.single '/') <|> P.eof)
    , P.single '/' $> AbsoluteContext
    , pure RelativeContext
    ]

resourceLocationG ∷ Gen ResourceLocation
resourceLocationG = do
  scheme ← QC.liftArbitrary schemeG
  context ← arbitrary
  path ← pathG
  pure ResourceLocation {scheme, context, path}

pathP ∷ Parser (Seq Text)
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

schemeP ∷ Parser Text
schemeP = fmap fst $ P.match $ schemeHeadP *> schemeTailP

schemeG ∷ Gen Text
schemeG = Text.cons <$> schemeHeadG <*> schemeTailG

schemeHeadP ∷ Parser Char
schemeHeadP = P.satisfy \x → Char.isAsciiLower x || Char.isAsciiUpper x

schemeHeadG ∷ Gen Char
schemeHeadG = QC.oneof [QC.choose ('a', 'z'), QC.choose ('A', 'Z')]

schemeTailP ∷ Parser Text
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

authorityP ∷ Parser Authority
authorityP = do
  userInfo ← P.optional $ P.try $ userInfoP <* P.single '@'
  host ← hostP
  port ← P.optional $ P.single ':' *> P.decimal
  pure Authority {userInfo, host, port}

authorityG ∷ Gen Authority
authorityG = do
  userInfo ← QC.liftArbitrary userInfoG
  host ← hostG
  port ← QC.liftArbitrary $ fmap fromIntegral $ arbitrary @Word16
  pure Authority {userInfo, host, port}

hostP ∷ Parser Text
hostP = ipv6P <|> ipv4P <|> regNameP

hostG ∷ Gen Text
hostG = QC.oneof [regNameG, ipv4G, ipv6G]

ipv6P ∷ Parser Text
ipv6P =
  fmap fst $
    P.match $
      P.single '['
        *> P.takeWhileP
          (Just "IpV6 character")
          ( \x →
              x == ':'
                || Char.isAsciiLower x
                || Char.isAsciiUpper x
                || Char.isDigit x
          )
        <* P.single ']'

ipv6G ∷ Gen Text
ipv6G =
  fmap (\x → "[" <> x <> "]") $
    fmap Text.pack $
      QC.listOf $
        QC.oneof
          [ pure ':'
          , QC.choose ('a', 'f')
          , QC.choose ('0', '9')
          , QC.choose ('A', 'F')
          ]

ipv4P ∷ Parser Text
ipv4P =
  fmap fst $
    P.match $
      P.satisfy Char.isDigit
        *> P.takeWhileP
          (Just "IPv4 character")
          ( \x →
              x == '.' || Char.isDigit x
          )

ipv4G ∷ Gen Text
ipv4G =
  fmap Text.pack $
    (:)
      <$> QC.choose ('0', '9')
      <*> QC.listOf (QC.oneof [QC.choose ('0', '9'), pure '.'])

regNameP ∷ Parser Text
regNameP =
  P.takeWhile1P
    (Just "registered name character")
    ( \x →
        Char.isAsciiLower x
          || Char.isAsciiUpper x
          || Char.isDigit x
          || List.elem @[] x "-._~%!$&'()*+,;="
    )

regNameG ∷ Gen Text
regNameG =
  fmap Text.pack $
    QC.listOf1 $
      QC.oneof
        [ QC.choose ('a', 'z')
        , QC.choose ('A', 'Z')
        , QC.choose ('0', '9')
        , QC.elements "-._~%!$&'()*+,;="
        ]

userInfoP ∷ Parser Text
userInfoP =
  P.takeWhileP
    (Just "user info character")
    ( \x →
        Char.isAsciiLower x
          || Char.isAsciiUpper x
          || Char.isDigit x
          || List.elem @[] x "-._~!$&'()*+,;="
    )

userInfoG ∷ Gen Text
userInfoG =
  fmap Text.pack $
    QC.listOf $
      QC.oneof
        [ QC.choose ('a', 'z')
        , QC.choose ('A', 'Z')
        , QC.choose ('0', '9')
        , QC.elements "-._~!$&'()*+,;="
        ]

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

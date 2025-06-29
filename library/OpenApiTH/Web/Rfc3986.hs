-- | Very straightforward translation of ABNF from
--   <https://www.ietf.org/rfc/rfc3986.txt>
module OpenApiTH.Web.Rfc3986 where

import Essentials

import Control.Applicative (Alternative (..), asum, liftA2)
import Control.Monad (mfilter, replicateM, replicateM_, unless)
import Control.Monad.Fail
import Control.Monad.Validate
import Data.Bifunctor (first)
import Data.Bits (shiftL, shiftR, toIntegralSized, (.&.))
import Data.Bool (not, (&&), (||))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (Char)
import Data.Char qualified as Char
import Data.Either (Either (..), either)
import Data.Foldable (fold, foldMap, foldl')
import Data.Function (const)
import Data.List qualified as List
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TB
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
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Show (show)
import Prelude (fromIntegral, (*), (+))

import OpenApiTH.Grammar
import OpenApiTH.Web.Rfc2234

data Uri = Uri
  { scheme ∷ Text
  , hierPart ∷ HierPart
  , query ∷ Maybe Text
  , fragment ∷ Maybe Text
  }
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary Uri

uriGrammar ∷ Grammar Uri
uriGrammar =
  label
    "URI"
    Grammar
      { render = \x →
          schemeGrammar.render x.scheme
            <> ":"
            <> hierPartGrammar.render x.hierPart
            <> foldMap (\q → "?" <> queryGrammar.render q) x.query
            <> foldMap (\f → "#" <> fragmentGrammar.render f) x.fragment
      , parser = do
          scheme ← schemeGrammar.parser
          P.single ':'
          hierPart ← hierPartGrammar.parser
          query ← P.optional $ P.single '?' *> queryGrammar.parser
          fragment ← P.optional $ P.single '#' *> fragmentGrammar.parser
          pure Uri {scheme, hierPart, query, fragment}
      , generator = arbitrary
      }

data HierPart
  = HierPart_Authority Authority PathAbempty
  | HierPart_Absolute PathAbsolute
  | HierPart_Rootless PathRootless
  | HierPart_Empty PathEmpty
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary HierPart

hierPartGrammar ∷ Grammar HierPart
hierPartGrammar =
  label
    "hier-part"
    Grammar
      { render = \case
          HierPart_Authority a p → "//" <> render a <> render p
          HierPart_Absolute p → render p
          HierPart_Rootless p → render p
          HierPart_Empty p → render p
      , parser =
          P.label "hier-part" $
            asum @[]
              [ P.chunk "//" *> (HierPart_Authority <$> parser <*> parser)
              , HierPart_Absolute <$> parser
              , HierPart_Rootless <$> parser
              , HierPart_Empty <$> parser
              ]
      , generator = arbitrary
      }

data UriReference
  = UriReference_Uri Uri
  | UriReference_RelativeRef RelativeRef
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary UriReference

uriReferenceGrammar =
  Grammar
    { render = \case
        UriReference_Uri x → render x
        UriReference_RelativeRef x → render x
    , parser =
        P.label "URI-reference" $
          asum @[]
            [ UriReference_Uri <$> parser
            , UriReference_RelativeRef <$> parser
            ]
    , generator = arbitrary
    }

data AbsoluteUri = AbsoluteUri
  { scheme ∷ Text
  , hierPart ∷ HierPart
  , query ∷ Maybe Text
  }
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary AbsoluteUri

absoluteUriGrammar =
  label
    "absolute-uri"
    Grammar
      { render = \x →
          render x.scheme
            <> ":"
            <> render x.hierPart
            <> foldMap (\q → "?" <> render q) x.query
      , parser = do
          scheme ← parser
          P.single ':'
          hierPart ← parser
          query ← P.optional $ P.single '?' *> parser
          pure AbsoluteUri {scheme, hierPart, query}
      , generator = arbitrary
      }

data RelativeRef = RelativeRef
  { relativePart ∷ RelativePart
  , query ∷ Maybe Text
  , fragment ∷ Maybe Text
  }
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary RelativeRef

relativeRefGrammar =
  label
    "relative-ref"
    Grammar
      { render = \x →
          render x.relativePart
            <> foldMap (\q → "?" <> render q) x.query
            <> foldMap (\f → "#" <> render f) x.fragment
      , parser = do
          relativePart ← parser
          query ← P.optional $ P.single '?' *> parser
          fragment ← P.optional $ P.single '#' *> parser
          pure RelativeRef {relativePart, query, fragment}
      , generator = arbitrary
      }

data RelativePart
  = RelativePart_Authority Authority PathAbempty
  | RelativePart_Absolute PathAbsolute
  | RelativePart_Noscheme PathNoscheme
  | RelativePart_Empty PathEmpty
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary RelativePart

relativePartGrammar =
  label
    "relative-part"
    Grammar
      { render = \case
          RelativePart_Authority a p → "//" <> render a <> render p
          RelativePart_Absolute p → render p
          RelativePart_Noscheme p → render p
          RelativePart_Empty p → render p
      , parser =
          asum @[]
            [ P.chunk "//" *> (RelativePart_Authority <$> parser <*> parser)
            , RelativePart_Absolute <$> parser
            , RelativePart_Noscheme <$> parser
            , RelativePart_Empty <$> parser
            ]
      , generator = arbitrary
      }

schemeGrammar ∷ Grammar Text
schemeGrammar =
  label
    "scheme"
    Grammar
      { render = TB.fromText
      , parser = fmap fst $ P.match do
          alphaGrammar.parser
          P.many $
            asum @[]
              [ void $ alpha.parser
              , void $ digit.parser
              , void $ schemeSymbolGrammar.parser
              ]
      , generator =
          fmap Text.pack $
            (:)
              <$> alpha.generator
              <*> QC.listOf
                ( QC.oneof
                    [ alpha.generator
                    , digit.generator
                    , schemeSymbolGrammar.generator
                    ]
                )
      }
 where
  schemeSymbolGrammar = tokenEnumeration "+-."

data Authority = Authority
  { userinfo ∷ Maybe Text
  , host ∷ Host
  , port ∷ Maybe Text
  }
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary Authority

authorityGrammar =
  label
    "authority"
    Grammar
      { render = \x →
          foldMap (\u → render u <> "@") x.userinfo
            <> render x.host
            <> foldMap (\p → ":" <> render p) x.port
      , parser = P.label "authority" do
          userinfo ← P.optional $ P.try $ parser <* P.single '@'
          host ← parser
          port ← P.optional $ P.single ':' *> parser
          pure Authority {userinfo, host, port}
      , generator = arbitrary
      }

userInfoGrammar =
  label
    "userinfo"
    Grammar
      { render = TB.fromText
      , parser = fmap fst $ P.match do
          P.many $
            asum @[]
              [ void $ unreservedGrammar.parser
              , void $ pctEncodedGrammar.parser
              , void $ subDelimGrammar.parser
              , void $ P.single ':'
              ]
      , generator =
          fmap (TL.toStrict . TB.toLazyText . fold) $
            QC.listOf $
              QC.oneof
                [ render <$> unreservedGrammar.generator
                , render <$> pctEncodedGrammar.generator
                , render <$> subDelimGrammar.generator
                , pure ":"
                ]
      }

data Host
  = Host_IpLiteral IpLiteral
  | Host_Ipv4 Text
  | Host_RegName Text
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary Host

hostGrammar =
  label
    "host"
    Grammar
      { render = \case
          Host_IpLiteral x → render x
          Host_Ipv4 x → render x
          Host_RegName x → render x
      , parser =
          asum @[]
            [ Host_IpLiteral <$> parser
            , Host_Ipv4 <$> parser
            , Host_RegName <$> parser
            ]
      , generator = arbitrary
      }

portGrammar =
  label
    "port"
    Grammar
      { render = TB.fromText
      , parser = fmap fst $ P.match $ P.many $ digitGrammar.parser
      , generator = fmap Text.pack $ QC.listOf $ fmap (.char) digitGrammar.generator
      }

data IpLiteral
  = IpLiteral_V6 Text
  | IpLiteral_Future Text
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary IpLiteral

ipLiteralGrammar =
  label
    "IP-literal"
    Grammar
      { render = \x →
          "["
            <> ( case x of
                  IpLiteral_V6 x → render x
                  IpLiteral_Future x → render x
               )
            <> "]"
      , parser =
          P.single '['
            *> asum @[]
              [ IpLiteral_V6 <$> parser
              , IpLiteral_Future <$> parser
              ]
            <* P.single ']'
      , generator = arbitrary
      }

ipvFutureGrammar ∷ Grammar Text
ipvFutureGrammar =
  label
    "IPvFuture"
    Grammar
      { render = TB.fromText
      , parser = fmap fst $ P.match do
          P.single 'v'
          hexdigGrammar.parser
          P.single '.'
          P.many $
            asum @[]
              [ void $ unreservedGrammar.parser
              , void $ subDelimGrammar.parser
              , void $ P.single ':'
              ]
      , generator = do
          x ← hexdigChar <$> hexdigGrammar.arbitrary
          xs ←
            QC.listOf1 $
              QC.oneof
                [ (.char) <$> unreservedGrammar.generator
                , (.char) <$> subDelimGrammar.generator
                , pure ':'
                ]
          pure $ IpvFutureUnsafe $ Text.pack $ ['v', x, '.'] <> xs
      }

-- | Parsing is much more lenient than the spec out of laziness, could be improved
ipv6AddressGrammar ∷ Grammar Text
ipv6AddressGrammar =
  label
    "IPv6address"
    Grammar
      { render = TB.fromText
      , parser =
          fmap fst $
            P.match $
              some $
                asum @[] [void hexdigGrammar.parser, void $ P.single ':']
      , generator =
          fmap (TL.toStrict . TB.toLazyText) $
            QC.oneof
              [ rep' 6 (h16 ^ pure ":") ^ ls32
              , pure "::" ^ rep' 5 (h16 ^ pure ":") ^ ls32
              , opt h16 ^ pure "::" ^ rep' 4 (h16 ^ pure ":") ^ ls32
              , opt (rep 0 1 (h16 ^ pure ":") ^ h16) ^ pure "::" ^ rep' 3 (h16 ^ pure ":") ^ ls32
              , opt (rep 0 2 (h16 ^ pure ":") ^ h16) ^ pure "::" ^ rep' 2 (h16 ^ pure ":") ^ ls32
              , opt (rep 0 3 (h16 ^ pure ":") ^ h16) ^ pure "::" ^ h16 ^ pure ":" ^ ls32
              , opt (rep 0 4 (h16 ^ pure ":") ^ h16) ^ pure "::" ^ ls32
              , opt (rep 0 5 (h16 ^ pure ":") ^ h16) ^ pure "::" ^ h16
              , opt (rep 0 6 (h16 ^ pure ":") ^ h16) ^ pure "::"
              ]
      }
 where
  rep a b g = do
    n ← QC.choose (a, b)
    fmap fold $ QC.vectorOf n g
  rep' a = rep a a
  (^) = liftA2 (<>)
  opt g = QC.oneof [pure "", g]
  h16, ls32 ∷ Gen TB.Builder
  h16 = do
    n ← QC.choose (1, 4)
    fmap TB.fromString $ QC.vectorOf n $ fmap hexdigChar hexdigGrammar.generator
  ls32 =
    QC.oneof
      [ h16 ^ pure ":" ^ h16
      , render <$> ipv4AddressGrammar.generator
      ]

ipv4AddressGrammar ∷ Grammar Text
ipv4AddressGrammar =
  label
    "IPv4address"
    Grammar
      { render = TB.fromText
      , parser = fmap fst $ P.match $ do
          decOctetGrammar.parser
          P.single '.'
          decOctetGrammar.parser
          P.single '.'
          decOctetGrammar.parser
          P.single '.'
          decOctetGrammar.parser
      , generator =
          fmap
            ( TL.toStrict
                . TB.toLazyText
                . fold
                . List.intersperse "."
                . fmap render
            )
            $ replicateM 4 gecOctetGrammar.arbitrary
      }

regNameGrammar =
  label
    "reg-name"
    Grammar
      { render = TB.fromText
      , parser =
          fmap fst $
            P.match $
              P.many $
                asum @[]
                  [ void $ unreservedGrammar.parser
                  , void $ pctEncodedGrammar.parser
                  , void $ subDelimGrammar.parser
                  ]
      , generator = _
      }

data Path
  = Path_Abempty PathAbempty
  | Path_Absolute PathAbsolute
  | Path_Noscheme PathNoscheme
  | Path_Rootless PathRootless
  | Path_Empty PathEmpty
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary Path

instance Grammar Path

newtype PathAbempty = PathAbempty [Segment]
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary PathAbempty

instance Grammar PathAbempty

data PathAbsolute = PathAbsolute SegmentNz [Segment]
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary PathAbsolute

instance Grammar PathAbsolute

data PathNoscheme = PathNoscheme SegmentNzNc [Segment]
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary PathNoscheme

instance Grammar PathNoscheme

data PathRootless = PathRootless SegmentNz [Segment]
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary PathRootless

instance Grammar PathRootless

data PathEmpty = PathEmpty
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary PathEmpty

instance Grammar PathEmpty

newtype Segment = SegmentUnsafe {text ∷ Text}

instance Grammar Segment

instance Arbitrary Segment

newtype SegmentNz = SegmentNzUnsafe {text ∷ Text}

instance Grammar SegmentNz

instance Arbitrary SegmentNz

newtype SegmentNzNc = SegmentNzNcUnsafe {text ∷ Text}

instance Grammar SegmentNzNc

instance Arbitrary SegmentNzNc

pctEncodedGrammar ∷ Grammar Word8
pctEncodedGrammar =
  label
    "pct-encoded"
    Grammar
      { render = \x →
          TB.singleton '%'
            <> render (HexdigUnsafe @'UpperCase $ x.byte `shiftR` 4)
            <> render (HexdigUnsafe @'UpperCase $ x.byte .&. 15)
      , parser = do
          P.single '%'
          a ← hexdigGrammar.parser
          b ← hexdigGrammar.parser
          pure $ PctEncoded $ (a.byte `shiftL` 4) + b.byte
      , generator = arbitrary
      }

unreservedGrammar ∷ Grammar Char
unreservedGrammar =
  label
    "unreserved"
    Grammar
      { render = TB.singleton
      , parser = alpha.parser <|> digit.parser <|> unreservedSymbolGrammar.parser
      , generator =
          QC.oneof
            [ alphaGrammar.generator
            , digitGrammar.generator
            , unreservedSymbolGrammar.generator
            ]
      }
 where
  unreservedSymbolGrammar = tokenEnumeration "-._~"

reservedGrammar = label "reserved" $ tokenEnumeration $ genDelims ++ subDelims

genDelimGrammar ∷ Grammar Char
genDelimGrammar = label "gen-delims" $ tokenEnumeration genDelims

genDelims = ":/?#[]@"

subDelimGrammar ∷ Grammar Char
subDelimGrammar = label "sub-delims" $ tokenEnumeration subDelims

subDelims = "!$&'()*+,;="

decOctetGrammar ∷ Grammar Word8
decOctetGrammar =
  label
    "dec-octet"
    Grammar
      { render = TB.decimal
      , parser = do
          xs ← some digitNumGrammar.parser
          let n ∷ Natural = foldl' (\t x → (t * 10) + fromIntegral x.byte) 0 xs
          maybe empty pure $ toIntegralSized n
      , generator = arbitrary
      }

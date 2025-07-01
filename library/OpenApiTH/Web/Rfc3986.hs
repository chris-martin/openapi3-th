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
import Test.QuickCheck (Gen, liftArbitrary)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary.Generic
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Show (show)
import Prelude (fromIntegral, (*), (+), (-))

import OpenApiTH.Grammar
import OpenApiTH.Web.Rfc2234

data Uri = Uri
  { scheme ∷ Text
  , hierPart ∷ HierPart
  , query ∷ Maybe Text
  , fragment ∷ Maybe Text
  }
  deriving Arbitrary via TheGrammar Uri

instance HasGrammar Uri where
  grammar =
    label
      "URI"
      Grammar
        { render = \x →
            schemeGrammar.render x.scheme
              <> ":"
              <> render grammar x.hierPart
              <> foldMap (\q → "?" <> queryGrammar.render q) x.query
              <> foldMap (\f → "#" <> fragmentGrammar.render f) x.fragment
        , parser = do
            scheme ← parser schemeGrammar
            P.single ':'
            hierPart ← parser grammar
            query ← P.optional $ P.single '?' *> parser queryGrammar
            fragment ← P.optional $ P.single '#' *> parser fragmentGrammar
            pure Uri {scheme, hierPart, query, fragment}
        , generator = do
            scheme ← schemeGrammar.generator
            hierPart ← generator grammar
            query ← liftArbitrary queryGrammar.generator
            fragment ← liftArbitrary fragmentGrammar.generator
            pure Uri {scheme, hierPart, query, fragment}
        }

data HierPart
  = HierPart_Authority Authority [Text]
  | HierPart_Absolute [Text]
  | HierPart_Rootless [Text]
  | HierPart_Empty
  deriving Arbitrary via TheGrammar HierPart

instance HasGrammar HierPart where
  grammar =
    label
      "hier-part"
      Grammar
        { render = \case
            HierPart_Authority a p → "//" <> render grammar a <> render pathAbemptyGrammar p
            HierPart_Absolute p → render pathAbsoluteGrammar p
            HierPart_Rootless p → render pathRootlessGrammar p
            HierPart_Empty → render pathEmptyGrammar ()
        , parser =
            P.label "hier-part" $
              asum @[]
                [ P.chunk "//" *> (HierPart_Authority <$> parser grammar <*> parser pathAbemptyGrammar)
                , HierPart_Absolute <$> parser pathAbsoluteGrammar
                , HierPart_Rootless <$> parser pathRootlessGrammar
                , HierPart_Empty <$ parser pathEmptyGrammar
                ]
        , generator =
            QC.oneof
              [ HierPart_Authority <$> generator grammar <*> generator pathAbemptyGrammar
              , HierPart_Absolute <$> generator pathAbsoluteGrammar
              , HierPart_Rootless <$> generator pathRootlessGrammar
              , HierPart_Empty <$ generator pathEmptyGrammar
              ]
        }

data UriReference
  = UriReference_Uri Uri
  | UriReference_RelativeRef RelativeRef
  deriving Arbitrary via TheGrammar UriReference

instance HasGrammar UriReference where
  grammar =
    Grammar
      { render = \case
          UriReference_Uri x → render grammar x
          UriReference_RelativeRef x → render grammar x
      , parser =
          P.label "URI-reference" $
            asum @[]
              [ UriReference_Uri <$> parser grammar
              , UriReference_RelativeRef <$> parser grammar
              ]
      , generator =
          QC.oneof
            [ UriReference_Uri <$> generator grammar
            , UriReference_RelativeRef <$> generator grammar
            ]
      }

data AbsoluteUri = AbsoluteUri
  { scheme ∷ Text
  , hierPart ∷ HierPart
  , query ∷ Maybe Text
  }
  deriving Arbitrary via TheGrammar AbsoluteUri

instance HasGrammar AbsoluteUri where
  grammar =
    label
      "absolute-uri"
      Grammar
        { render = \x →
            render schemeGrammar x.scheme
              <> ":"
              <> render grammar x.hierPart
              <> foldMap (\q → "?" <> render queryGrammar q) x.query
        , parser = do
            scheme ← parser schemeGrammar
            P.single ':'
            hierPart ← parser grammar
            query ← P.optional $ P.single '?' *> parser queryGrammar
            pure AbsoluteUri {scheme, hierPart, query}
        , generator = do
            scheme ← schemeGrammar.generator
            hierPart ← generator grammar
            query ← liftArbitrary $ generator queryGrammar
            pure AbsoluteUri {scheme, hierPart, query}
        }

data RelativeRef = RelativeRef
  { relativePart ∷ RelativePart
  , query ∷ Maybe Text
  , fragment ∷ Maybe Text
  }
  deriving stock Generic
  deriving Arbitrary via TheGrammar RelativeRef

instance HasGrammar RelativeRef where
  grammar =
    label
      "relative-ref"
      Grammar
        { render = \x →
            render grammar x.relativePart
              <> foldMap (\q → "?" <> render queryGrammar q) x.query
              <> foldMap (\f → "#" <> render fragmentGrammar f) x.fragment
        , parser = do
            relativePart ← parser grammar
            query ← P.optional $ P.single '?' *> parser queryGrammar
            fragment ← P.optional $ P.single '#' *> parser fragmentGrammar
            pure RelativeRef {relativePart, query, fragment}
        , generator = do
            relativePart ← generator grammar
            query ← liftArbitrary $ generator queryGrammar
            fragment ← liftArbitrary $ generator fragmentGrammar
            pure RelativeRef {relativePart, query, fragment}
        }

data RelativePart
  = RelativePart_Authority Authority [Text]
  | RelativePart_Absolute [Text]
  | RelativePart_Noscheme [Text]
  | RelativePart_Empty
  deriving stock Generic
  deriving Arbitrary via TheGrammar RelativePart

instance HasGrammar RelativePart where
  grammar =
    label
      "relative-part"
      Grammar
        { render = \case
            RelativePart_Authority a p → "//" <> render grammar a <> render pathAbemptyGrammar p
            RelativePart_Absolute p → render pathAbsoluteGrammar p
            RelativePart_Noscheme p → render pathNoschemeGrammar p
            RelativePart_Empty → render pathEmptyGrammar ()
        , parser =
            asum @[]
              [ P.chunk "//" *> (RelativePart_Authority <$> parser grammar <*> parser pathAbemptyGrammar)
              , RelativePart_Absolute <$> parser pathAbsoluteGrammar
              , RelativePart_Noscheme <$> parser pathNoschemeGrammar
              , RelativePart_Empty <$ parser pathEmptyGrammar
              ]
        , generator =
            QC.oneof
              [ RelativePart_Authority <$> generator grammar <*> generator pathAbemptyGrammar
              , RelativePart_Absolute <$> generator pathAbsoluteGrammar
              , RelativePart_Noscheme <$> generator pathNoschemeGrammar
              , RelativePart_Empty <$ generator pathEmptyGrammar
              ]
        }

schemeGrammar ∷ Grammar Text
schemeGrammar =
  label "scheme" $
    textGrammar
      ( do
          alphaGrammar.parser
          P.many $
            asum @[]
              [ void $ parser alphaGrammar
              , void $ parser digitGrammar
              , void $ parser schemeSymbolGrammar
              ]
          pure ()
      )
      ( fmap TB.fromString $
          (:)
            <$> generator alphaGrammar
            <*> QC.listOf
              ( QC.oneof
                  [ generator alphaGrammar
                  , generator digitGrammar
                  , generator schemeSymbolGrammar
                  ]
              )
      )
 where
  schemeSymbolGrammar = tokenEnumeration "+-."

data Authority = Authority
  { userinfo ∷ Maybe Text
  , host ∷ Host
  , port ∷ Maybe Text
  }
  deriving Arbitrary via TheGrammar Authority

instance HasGrammar Authority where
  grammar =
    label
      "authority"
      Grammar
        { render = \x →
            foldMap (\u → render userinfoGrammar u <> "@") x.userinfo
              <> render grammar x.host
              <> foldMap (\p → ":" <> render portGrammar p) x.port
        , parser = P.label "authority" do
            userinfo ← P.optional $ P.try $ parser userinfoGrammar <* P.single '@'
            host ← parser grammar
            port ← P.optional $ P.single ':' *> parser portGrammar
            pure Authority {userinfo, host, port}
        , generator = do
            userinfo ← liftArbitrary $ generator userinfoGrammar
            host ← generator grammar
            port ← liftArbitrary $ generator portGrammar
            pure Authority {userinfo, host, port}
        }

userinfoGrammar ∷ Grammar Text
userinfoGrammar =
  label "userinfo" $
    textGrammar
      ( do
          P.many $
            asum @[]
              [ void $ unreservedGrammar.parser
              , void $ pctEncodedGrammar.parser
              , void $ subDelimGrammar.parser
              , void $ P.single ':'
              ]
          pure ()
      )
      ( fmap fold $
          QC.listOf $
            QC.oneof
              [ renderGenerator unreservedGrammar
              , renderGenerator pctEncodedGrammar
              , renderGenerator subDelimGrammar
              , pure ":"
              ]
      )

data Host
  = Host_IpLiteral IpLiteral
  | Host_Ipv4 Text
  | Host_RegName Text
  deriving Arbitrary via TheGrammar Host

instance HasGrammar Host where
  grammar =
    label
      "host"
      Grammar
        { render = \case
            Host_IpLiteral x → render grammar x
            Host_Ipv4 x → render ipv4AddressGrammar x
            Host_RegName x → render regNameGrammar x
        , parser =
            asum @[]
              [ Host_IpLiteral <$> parser grammar
              , Host_Ipv4 <$> parser ipv4AddressGrammar
              , Host_RegName <$> parser regNameGrammar
              ]
        , generator =
            QC.oneof
              [ Host_IpLiteral <$> generator grammar
              , Host_Ipv4 <$> generator ipv4AddressGrammar
              , Host_RegName <$> generator regNameGrammar
              ]
        }

portGrammar ∷ Grammar Text
portGrammar =
  label "port" $
    textGrammar
      (void $ P.many $ parser digitGrammar)
      (fmap TB.fromString $ QC.listOf $ generator digitGrammar)

data IpLiteral
  = IpLiteral_V6 Text
  | IpLiteral_Future Text
  deriving stock Generic
  deriving Arbitrary via TheGrammar IpLiteral

instance HasGrammar IpLiteral where
  grammar =
    label
      "IP-literal"
      Grammar
        { render = \x →
            "["
              <> ( case x of
                    IpLiteral_V6 x → render ipv6AddressGrammar x
                    IpLiteral_Future x → render ipvFutureGrammar x
                 )
              <> "]"
        , parser =
            P.single '['
              *> asum @[]
                [ IpLiteral_V6 <$> parser ipv6AddressGrammar
                , IpLiteral_Future <$> parser ipvFutureGrammar
                ]
              <* P.single ']'
        , generator =
            QC.oneof
              [ IpLiteral_V6 <$> generator ipv6AddressGrammar
              , IpLiteral_Future <$> generator ipvFutureGrammar
              ]
        }

ipvFutureGrammar ∷ Grammar Text
ipvFutureGrammar =
  label "IPvFuture" $
    textGrammar
      ( do
          P.single 'v'
          hexdigGrammar.parser
          P.single '.'
          P.many $
            asum @[]
              [ void $ unreservedGrammar.parser
              , void $ subDelimGrammar.parser
              , void $ P.single ':'
              ]
          pure ()
      )
      ( do
          x ← generator hexdigGrammar
          xs ←
            QC.listOf1 $
              QC.oneof
                [ generator unreservedGrammar
                , generator subDelimGrammar
                , pure ':'
                ]
          pure $ TB.fromString $ ['v', x, '.'] <> xs
      )

-- | Parsing is much more lenient than the spec out of laziness, could be improved
ipv6AddressGrammar ∷ Grammar Text
ipv6AddressGrammar =
  label "IPv6address" $
    textGrammar
      ( void $
          some $
            asum @[] [void hexdigGrammar.parser, void $ P.single ':']
      )
      ( QC.oneof
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
      )
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
    fmap TB.fromString $ QC.vectorOf n hexdigGrammar.generator
  ls32 =
    QC.oneof
      [ h16 ^ pure ":" ^ h16
      , renderGenerator ipv4AddressGrammar
      ]

ipv4AddressGrammar ∷ Grammar Text
ipv4AddressGrammar =
  label "IPv4address" $
    textGrammar
      ( do
          decOctetGrammar.parser
          P.single '.'
          decOctetGrammar.parser
          P.single '.'
          decOctetGrammar.parser
          P.single '.'
          decOctetGrammar.parser
          pure ()
      )
      ( fmap (fold . List.intersperse ".") $
          replicateM 4 (renderGenerator decOctetGrammar)
      )

regNameGrammar ∷ Grammar Text
regNameGrammar =
  label "reg-name" $
    textGrammar
      ( void $
          P.many $
            asum @[]
              [ void $ parser unreservedGrammar
              , void $ parser pctEncodedGrammar
              , void $ parser subDelimGrammar
              ]
      )
      ( fmap fold $
          QC.listOf $
            QC.oneof
              [ renderGenerator unreservedGrammar
              , renderGenerator pctEncodedGrammar
              , renderGenerator subDelimGrammar
              ]
      )

data Path
  = Path_Abempty [Text] -- PathAbempty
  | Path_Absolute [Text] -- PathAbsolute
  | Path_Noscheme [Text] -- PathNoscheme
  | Path_Rootless [Text] -- PathRootless
  | Path_Empty -- PathEmpty
  deriving Arbitrary via TheGrammar Path

instance HasGrammar Path where
  grammar =
    Grammar
      { render = _
      , parser = _
      , generator = _
      }

pathAbemptyGrammar ∷ Grammar [Text]
pathAbemptyGrammar =
  label
    "path-abempty"
    Grammar
      { render = foldMap (\s → "/" <> segmentGrammar.render s)
      , parser = many (P.single '/' *> segmentGrammar.parser)
      , generator = QC.listOf segmentGrammar.generator
      }

pathAbsoluteGrammar ∷ Grammar [Text]
pathAbsoluteGrammar =
  label
    "path-absolute"
    Grammar
      { render = \ss →
          "/" <> case ss of
            [] → mempty
            x : xs →
              segmentNzGrammar.render x
                <> foldMap (\s → "/" <> segmentGrammar.render s) xs
      , parser = do
          P.single '/'
          x ← segmentNzGrammar.parser
          xs ← many (P.single '/' *> segmentGrammar.parser)
          pure $ x : xs
      , generator = QC.sized \size → do
          n ← QC.choose (0, size)
          case n of
            0 → pure []
            n →
              (:)
                <$> segmentNzGrammar.generator
                <*> QC.vectorOf (n - 1) segmentGrammar.generator
      }

-- data PathNoscheme = PathNoscheme SegmentNzNc [Segment]
pathNoschemeGrammar ∷ Grammar [Text]
pathNoschemeGrammar =
  Grammar
    { render = _
    , parser = _
    , generator = _
    }

-- data PathRootless = PathRootless SegmentNz [Segment]
pathRootlessGrammar ∷ Grammar [Text]
pathRootlessGrammar =
  Grammar
    { render = _
    , parser = _
    , generator = _
    }

pathEmptyGrammar ∷ Grammar ()
pathEmptyGrammar =
  Grammar
    { render = _
    , parser = _
    , generator = _
    }

segmentGrammar ∷ Grammar Text
segmentGrammar =
  Grammar
    { render = _
    , parser = _
    , generator = _
    }

segmentNzGrammar ∷ Grammar Text
segmentNzGrammar =
  Grammar
    { render = _
    , parser = _
    , generator = _
    }

segmentNzNcGrammar ∷ Grammar Text
segmentNzNcGrammar =
  Grammar
    { render = _
    , parser = _
    , generator = _
    }

pctEncodedGrammar ∷ Grammar Word8
pctEncodedGrammar =
  label
    "pct-encoded"
    Grammar
      { render = \x →
          TB.singleton '%'
            <> render (hexdigNumGrammar UpperCase) (x `shiftR` 4)
            <> render (hexdigNumGrammar UpperCase) (x .&. 15)
      , parser = do
          P.single '%'
          a ← parser $ hexdigNumGrammar UpperCase
          b ← parser $ hexdigNumGrammar UpperCase
          pure $ (a `shiftL` 4) + b
      , generator = arbitrary
      }

unreservedGrammar ∷ Grammar Char
unreservedGrammar =
  label
    "unreserved"
    Grammar
      { render = TB.singleton
      , parser =
          asum @[]
            [ parser alphaGrammar
            , parser digitGrammar
            , parser unreservedSymbolGrammar
            ]
      , generator =
          QC.oneof
            [ alphaGrammar.generator
            , digitGrammar.generator
            , unreservedSymbolGrammar.generator
            ]
      }
 where
  unreservedSymbolGrammar = tokenEnumeration "-._~"

reservedGrammar = label "reserved" $ tokenEnumeration $ genDelims <> subDelims

genDelimGrammar ∷ Grammar Char
genDelimGrammar = label "gen-delims" $ tokenEnumeration genDelims

genDelims ∷ [Char]
genDelims = ":/?#[]@"

subDelimGrammar ∷ Grammar Char
subDelimGrammar = label "sub-delims" $ tokenEnumeration subDelims

subDelims ∷ [Char]
subDelims = "!$&'()*+,;="

decOctetGrammar ∷ Grammar Word8
decOctetGrammar =
  label
    "dec-octet"
    Grammar
      { render = TB.decimal
      , parser = do
          xs ← some digitNumGrammar.parser
          let n ∷ Natural = foldl' (\t x → (t * 10) + fromIntegral x) 0 xs
          maybe empty pure $ toIntegralSized n
      , generator = arbitrary
      }

queryGrammar ∷ Grammar Text
queryGrammar = _

fragmentGrammar ∷ Grammar Text
fragmentGrammar = _

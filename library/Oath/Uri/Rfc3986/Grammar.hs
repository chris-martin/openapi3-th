-- | Very straightforward translation of ABNF from
--   <https://www.ietf.org/rfc/rfc3986.txt>
module Oath.Uri.Rfc3986.Grammar (
  Uri (..),
  HierPart (..),
  UriReference (..),
  AbsoluteUri (..),
  RelativeRef (..),
  RelativePart (..),
  schemeGrammar,
  Authority (..),
  userinfoGrammar,
  Host (..),
  portGrammar,
  IpLiteral (..),
  ipvFutureGrammar,
  ipv6AddressGrammar,
  ipv4AddressGrammar,
  regNameGrammar,
  pathAbemptyGrammar,
  pathNoschemeGrammar,
  pathRootlessGrammar,
  pathEmptyGrammar,
  segmentGrammar,
  segmentNzGrammar,
  segmentNzNcGrammar,
  pctEncodedGrammar,
  unreservedGrammar,
  reservedGrammar,
  genDelimGrammar,
  subDelimGrammar,
  decOctetGrammar,
  queryGrammar,
  fragmentGrammar,
  pcharGrammar,
) where

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
import Data.ByteString.Builder qualified as BSB
import Data.Char (Char)
import Data.Char qualified as Char
import Data.Either (Either (..), either)
import Data.Foldable (fold, foldMap, foldl')
import Data.Function (const)
import Data.List qualified as List
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq (..))
import Data.Sequence.NonEmpty qualified as NESeq
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

import Data.ByteString.Builder (Builder)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Oath.Abnf.Rfc2234
import Oath.Grammar

data Uri = Uri
  { scheme ∷ ByteString
  , hierPart ∷ HierPart
  , query ∷ Maybe ByteString
  , fragment ∷ Maybe ByteString
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
            P.single $ char ':'
            hierPart ← parser grammar
            query ← P.optional $ P.single (char '?') *> parser queryGrammar
            fragment ← P.optional $ P.single (char '#') *> parser fragmentGrammar
            pure Uri {scheme, hierPart, query, fragment}
        , generator = do
            scheme ← schemeGrammar.generator
            hierPart ← generator grammar
            query ← liftArbitrary queryGrammar.generator
            fragment ← liftArbitrary fragmentGrammar.generator
            pure Uri {scheme, hierPart, query, fragment}
        }

data HierPart
  = HierPart_Authority Authority (Seq ByteString)
  | HierPart_Absolute (Seq ByteString)
  | HierPart_Rootless (NESeq ByteString)
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
  { scheme ∷ ByteString
  , hierPart ∷ HierPart
  , query ∷ Maybe ByteString
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
            P.single $ char ':'
            hierPart ← parser grammar
            query ← P.optional $ P.single (char '?') *> parser queryGrammar
            pure AbsoluteUri {scheme, hierPart, query}
        , generator = do
            scheme ← schemeGrammar.generator
            hierPart ← generator grammar
            query ← liftArbitrary $ generator queryGrammar
            pure AbsoluteUri {scheme, hierPart, query}
        }

data RelativeRef = RelativeRef
  { relativePart ∷ RelativePart
  , query ∷ Maybe ByteString
  , fragment ∷ Maybe ByteString
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
            query ← P.optional $ P.single (char '?') *> parser queryGrammar
            fragment ← P.optional $ P.single (char '#') *> parser fragmentGrammar
            pure RelativeRef {relativePart, query, fragment}
        , generator = do
            relativePart ← generator grammar
            query ← liftArbitrary $ generator queryGrammar
            fragment ← liftArbitrary $ generator fragmentGrammar
            pure RelativeRef {relativePart, query, fragment}
        }

data RelativePart
  = RelativePart_Authority Authority (Seq ByteString)
  | RelativePart_Absolute (Seq ByteString)
  | RelativePart_Noscheme (NESeq ByteString)
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

schemeGrammar ∷ Grammar ByteString
schemeGrammar =
  label "scheme" $
    textGrammar
      ( do
          alphaGrammar.parser
          P.many $
            asum @[]
              [ void $ parser alphaGrammar
              , void $ parser digitGrammar
              , void $ parser etc
              ]
          pure ()
      )
      ( fmap fold $
          (:)
            <$> (BSB.word8 <$> generator alphaGrammar)
            <*> QC.listOf
              ( BSB.word8
                  <$> QC.oneof
                    [ generator alphaGrammar
                    , generator digitGrammar
                    , generator etc
                    ]
              )
      )
 where
  etc = tokenEnumeration $ char <$> "+-."

data Authority = Authority
  { userinfo ∷ Maybe ByteString
  , host ∷ Host
  , port ∷ Maybe ByteString
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
            userinfo ← P.optional $ P.try $ parser userinfoGrammar <* P.single (char '@')
            host ← parser grammar
            port ← P.optional $ P.single (char ':') *> parser portGrammar
            pure Authority {userinfo, host, port}
        , generator = do
            userinfo ← liftArbitrary $ generator userinfoGrammar
            host ← generator grammar
            port ← liftArbitrary $ generator portGrammar
            pure Authority {userinfo, host, port}
        }

userinfoGrammar ∷ Grammar ByteString
userinfoGrammar =
  label "userinfo" $
    textGrammar
      ( do
          P.many $
            asum @[]
              [ void $ unreservedGrammar.parser
              , void $ pctEncodedGrammar.parser
              , void $ subDelimGrammar.parser
              , void $ P.single $ char ':'
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
  | Host_Ipv4 ByteString
  | Host_RegName ByteString
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

portGrammar ∷ Grammar ByteString
portGrammar =
  label "port" $
    textGrammar
      (void $ P.many $ parser digitGrammar)
      (fmap fold $ QC.listOf $ BSB.word8 <$> generator digitGrammar)

data IpLiteral
  = IpLiteral_V6 ByteString
  | IpLiteral_Future ByteString
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
            P.single (char '[')
              *> asum @[]
                [ IpLiteral_V6 <$> parser ipv6AddressGrammar
                , IpLiteral_Future <$> parser ipvFutureGrammar
                ]
              <* P.single (char ']')
        , generator =
            QC.oneof
              [ IpLiteral_V6 <$> generator ipv6AddressGrammar
              , IpLiteral_Future <$> generator ipvFutureGrammar
              ]
        }

ipvFutureGrammar ∷ Grammar ByteString
ipvFutureGrammar =
  label "IPvFuture" $
    textGrammar
      ( do
          P.single $ char 'v'
          hexdigGrammar.parser
          P.single $ char '.'
          P.many $
            asum @[]
              [ void $ unreservedGrammar.parser
              , void $ subDelimGrammar.parser
              , void $ P.single $ char ':'
              ]
          pure ()
      )
      ( do
          x ← BSB.word8 <$> generator hexdigGrammar
          xs ←
            fmap fold $
              QC.listOf1 $
                BSB.word8
                  <$> QC.oneof
                    [ generator unreservedGrammar
                    , generator subDelimGrammar
                    , pure $ char ':'
                    ]
          pure $ "v" <> x <> "." <> xs
      )

-- | Parsing is much more lenient than the spec out of laziness, could be improved
ipv6AddressGrammar ∷ Grammar ByteString
ipv6AddressGrammar =
  label "IPv6address" $
    textGrammar
      ( void $
          some $
            asum @[]
              [ void hexdigGrammar.parser
              , void $ P.single $ char ':'
              ]
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
  h16, ls32 ∷ Gen Builder
  h16 = do
    n ← QC.choose (1, 4)
    fmap fold $ QC.vectorOf n $ BSB.word8 <$> hexdigGrammar.generator
  ls32 =
    QC.oneof
      [ h16 ^ pure ":" ^ h16
      , renderGenerator ipv4AddressGrammar
      ]

ipv4AddressGrammar ∷ Grammar ByteString
ipv4AddressGrammar =
  label "IPv4address" $
    textGrammar
      ( do
          let o = decOctetGrammar.parser
              d = P.single $ char '.'
          o *> d *> o *> d *> o *> d *> o
          pure ()
      )
      ( fmap (fold . List.intersperse ".") $
          replicateM 4 (renderGenerator decOctetGrammar)
      )

regNameGrammar ∷ Grammar ByteString
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

pathAbemptyGrammar ∷ Grammar (Seq ByteString)
pathAbemptyGrammar =
  label
    "path-abempty"
    Grammar
      { render = foldMap (\s → "/" <> segmentGrammar.render s)
      , parser = fmap Seq.fromList $ many $ P.single (char '/') *> segmentGrammar.parser
      , generator = fmap Seq.fromList $ QC.listOf segmentGrammar.generator
      }

pathAbsoluteGrammar ∷ Grammar (Seq ByteString)
pathAbsoluteGrammar =
  label
    "path-absolute"
    Grammar
      { render = \ss →
          "/" <> case ss of
            Empty → mempty
            x :<| xs →
              segmentNzGrammar.render x
                <> foldMap (\s → "/" <> segmentGrammar.render s) xs
      , parser = do
          P.single $ char '/'
          x ← segmentNzGrammar.parser
          xs ← fmap Seq.fromList $ many $ P.single (char '/') *> segmentGrammar.parser
          pure $ x :<| xs
      , generator = QC.sized \size → do
          n ← QC.choose (0, size)
          case n of
            0 → pure []
            n →
              (:<|)
                <$> segmentNzGrammar.generator
                <*> fmap Seq.fromList (QC.vectorOf (n - 1) segmentGrammar.generator)
      }

pathNoschemeGrammar ∷ Grammar (NESeq ByteString)
pathNoschemeGrammar =
  label
    "path-noscheme"
    Grammar
      { render = \(x :<|| xs) →
          render segmentNzNcGrammar x
            <> foldMap (\s → "/" <> render segmentGrammar s) xs
      , parser = do
          x ← parser segmentNzNcGrammar
          xs ← fmap Seq.fromList $ P.many $ P.single (char '/') *> parser segmentGrammar
          pure $ x :<|| xs
      , generator = do
          x ← generator segmentNzNcGrammar
          xs ← fmap Seq.fromList $ QC.listOf $ generator segmentGrammar
          pure $ x :<|| xs
      }

pathRootlessGrammar ∷ Grammar (NESeq ByteString)
pathRootlessGrammar =
  label
    "path-rootless"
    Grammar
      { render = \(x :<|| xs) →
          render segmentNzGrammar x
            <> foldMap (\s → "/" <> render segmentGrammar s) xs
      , parser = do
          x ← parser segmentNzGrammar
          xs ← fmap Seq.fromList $ P.many $ P.single (char '/') *> parser segmentGrammar
          pure $ x :<|| xs
      , generator = do
          x ← generator segmentNzGrammar
          xs ← fmap Seq.fromList $ QC.listOf $ generator segmentGrammar
          pure $ x :<|| xs
      }

pathEmptyGrammar ∷ Grammar ()
pathEmptyGrammar =
  Grammar
    { render = const mempty
    , parser = pure ()
    , generator = pure ()
    }

segmentGrammar ∷ Grammar ByteString
segmentGrammar =
  label "segment" $
    textGrammar
      (void $ P.many $ parser pcharGrammar)
      (fmap fold $ QC.listOf $ renderGenerator pcharGrammar)

segmentNzGrammar ∷ Grammar ByteString
segmentNzGrammar =
  label "segment-nz" $
    textGrammar
      (void $ P.some $ parser pcharGrammar)
      (fmap fold $ QC.listOf1 $ renderGenerator pcharGrammar)

segmentNzNcGrammar ∷ Grammar ByteString
segmentNzNcGrammar =
  label "segment-nz-nc" $
    textGrammar
      (void $ P.some $ parser segmentNcCharGrammar)
      (fmap fold $ QC.listOf1 $ renderGenerator segmentNcCharGrammar)
 where
  segmentNcCharGrammar =
    textGrammar
      ( asum @[]
          [ void unreservedGrammar.parser
          , void pctEncodedGrammar.parser
          , void subDelimGrammar.parser
          , void etc.parser
          ]
      )
      ( QC.oneof
          [ renderGenerator unreservedGrammar
          , renderGenerator pctEncodedGrammar
          , renderGenerator subDelimGrammar
          , renderGenerator etc
          ]
      )
  etc = tokenEnumeration $ char <$> ":"

pctEncodedGrammar ∷ Grammar Word8
pctEncodedGrammar =
  label
    "pct-encoded"
    Grammar
      { render = \x →
          BSB.word8 (char '%')
            <> render (hexdigNumGrammar UpperCase) (x `shiftR` 4)
            <> render (hexdigNumGrammar UpperCase) (x .&. 15)
      , parser = do
          P.single $ char '%'
          a ← parser $ hexdigNumGrammar UpperCase
          b ← parser $ hexdigNumGrammar UpperCase
          pure $ (a `shiftL` 4) + b
      , generator = arbitrary
      }

unreservedGrammar ∷ Grammar Word8
unreservedGrammar =
  label
    "unreserved"
    Grammar
      { render = BSB.word8
      , parser =
          asum @[]
            [ parser alphaGrammar
            , parser digitGrammar
            , parser etc
            ]
      , generator =
          QC.oneof
            [ generator alphaGrammar
            , generator digitGrammar
            , generator etc
            ]
      }
 where
  etc = tokenEnumeration $ char <$> "-._~"

reservedGrammar ∷ Grammar Word8
reservedGrammar = label "reserved" $ tokenEnumeration $ genDelims <> subDelims

genDelimGrammar ∷ Grammar Word8
genDelimGrammar = label "gen-delims" $ tokenEnumeration genDelims

genDelims ∷ [Word8]
genDelims = char <$> ":/?#[]@"

subDelimGrammar ∷ Grammar Word8
subDelimGrammar = label "sub-delims" $ tokenEnumeration subDelims

subDelims ∷ [Word8]
subDelims = char <$> "!$&'()*+,;="

decOctetGrammar ∷ Grammar Word8
decOctetGrammar =
  label
    "dec-octet"
    Grammar
      { render = BSB.word8Dec
      , parser = do
          xs ← some digitNumGrammar.parser
          let n ∷ Natural = foldl' (\t x → (t * 10) + fromIntegral x) 0 xs
          maybe empty pure $ toIntegralSized n
      , generator = arbitrary
      }

queryGrammar ∷ Grammar ByteString
queryGrammar = label "query" queryOrFragmentGrammar

fragmentGrammar ∷ Grammar ByteString
fragmentGrammar = label "fragment" queryOrFragmentGrammar

queryOrFragmentGrammar ∷ Grammar ByteString
queryOrFragmentGrammar =
  textGrammar
    ( void $
        P.many $
          asum @[]
            [ void $ parser pcharGrammar
            , void $ parser etc
            ]
    )
    ( fmap fold $
        QC.listOf $
          QC.oneof
            [ renderGenerator pcharGrammar
            , renderGenerator etc
            ]
    )
 where
  etc = tokenEnumeration $ char <$> "/?"

pcharGrammar ∷ Grammar ByteString
pcharGrammar =
  label "pchar" $
    textGrammar
      ( asum @[]
          [ void unreservedGrammar.parser
          , void pctEncodedGrammar.parser
          , void subDelimGrammar.parser
          , void etc.parser
          ]
      )
      ( QC.oneof
          [ renderGenerator unreservedGrammar
          , renderGenerator pctEncodedGrammar
          , renderGenerator subDelimGrammar
          , renderGenerator etc
          ]
      )
 where
  etc = tokenEnumeration $ char <$> ":@"

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
  IpvFuture (..),
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
import Control.Monad (mfilter, replicateM, replicateM_, sequence, unless)
import Control.Monad.Fail
import Control.Monad.Validate
import Data.Bifunctor (first)
import Data.Bits (shiftL, shiftR, toIntegralSized, (.&.))
import Data.Bool (not, (&&), (||))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL
import Data.Char (Char)
import Data.Char qualified as Char
import Data.Either (Either (..), either)
import Data.Foldable (fold, foldMap, foldl')
import Data.Function (const)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
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

import Oath.Abnf.Rfc2234
import Oath.Grammar
import Oath.Uri.Rfc3986.Appendages
import Oath.Uri.Rfc3986.Characters
import Oath.Uri.Rfc3986.Host
import Oath.Uri.Rfc3986.Scheme
import Oath.Uri.Rfc3986.Segment

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
        { render =
            renderChoices
              [ render schemeGrammar x.scheme
              , ":"
              , render grammar x.hierPart
              , foldMap (\q → "?" <> render queryGrammar q) x.query
              , foldMap (\f → "#" <> render fragmentGrammar f) x.fragment
              ]
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
        { render = r
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
   where
    r = \case
      HierPart_Authority a p → "//" <> render grammar a <> render pathAbemptyGrammar p
      HierPart_Absolute p → render pathAbsoluteGrammar p
      HierPart_Rootless p → render pathRootlessGrammar p
      HierPart_Empty → render pathEmptyGrammar ()

data UriReference
  = UriReference_Uri Uri
  | UriReference_RelativeRef RelativeRef
  deriving Arbitrary via TheGrammar UriReference

instance HasGrammar UriReference where
  grammar =
    Grammar
      { render = r
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
   where
    r = \case
      UriReference_Uri x → render grammar x
      UriReference_RelativeRef x → render grammar x

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
        { render = r
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
   where
    r x =
      render schemeGrammar x.scheme
        <> ":"
        <> render grammar x.hierPart
        <> foldMap (\q → "?" <> render queryGrammar q) x.query

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
        { render = r
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
   where
    r x =
      render grammar x.relativePart
        <> foldMap (\q → "?" <> render queryGrammar q) x.query
        <> foldMap (\f → "#" <> render fragmentGrammar f) x.fragment

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
        { render = r
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
   where
    r = \case
      RelativePart_Authority a p → "//" <> render grammar a <> render pathAbemptyGrammar p
      RelativePart_Absolute p → render pathAbsoluteGrammar p
      RelativePart_Noscheme p → render pathNoschemeGrammar p
      RelativePart_Empty → render pathEmptyGrammar ()

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
        { render = r
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
   where
    r x =
      foldMap (\u → render userinfoGrammar u <> "@") x.userinfo
        <> render grammar x.host
        <> foldMap (\p → ":" <> render portGrammar p) x.port

userinfoGrammar ∷ Grammar ByteString
userinfoGrammar =
  label "userinfo" $
    Grammar
      { render = BSB.byteString
      , parser =
          fmap (build . fold) $
            P.many $
              asum @[]
                [ fmap BSB.word8 $ unreservedGrammar.parser
                , fmap BSB.word8 $ pctEncodedGrammar.parser
                , fmap BSB.word8 $ subDelimGrammar.parser
                , fmap BSB.word8 $ P.single $ char ':'
                ]
      , generator =
          fmap (build . fold) $
            QC.listOf $
              QC.oneof
                [ renderGenerator unreservedGrammar
                , renderGenerator pctEncodedGrammar
                , renderGenerator subDelimGrammar
                , pure ":"
                ]
      }

portGrammar ∷ Grammar ByteString
portGrammar =
  label
    "port"
    Grammar
      { render = BSB.byteString
      , parser = fmap fst $ P.match $ P.many $ parser digitGrammar
      , generator =
          fmap (build . fold) $
            QC.listOf $
              BSB.word8 <$> generator digitGrammar
      }

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
    { render = r
    , parser = pure ()
    , generator = pure ()
    }
 where
  r = const mempty

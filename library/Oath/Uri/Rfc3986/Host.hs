module Oath.Uri.Rfc3986.Host where

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
import Oath.Uri.Rfc3986.Characters

data Host
  = Host_IpLiteral IpLiteral
  | Host_Ipv4 ByteString
  | Host_RegName ByteString

data IpLiteral
  = IpLiteral_V6 ByteString
  | IpLiteral_Future IpvFuture
  deriving stock Generic

data IpvFuture = IpvFuture
  { version ∷ Word8
  , address ∷ ByteString
  }

makePrismLabels ''Host
makePrismLabels ''IpLiteral
makeFieldLabels ''IpvFuture

instance HasGrammar Host where
  grammar =
    label "host" $
      grammarAlternatives
        [ prismGrammar #_Host_IpLiteral grammar
        , prismGrammar #_Host_Ipv4 ipv4AddressGrammar
        , prismGrammar #_Host_RegName regNameGrammar
        ]

instance HasGrammar IpLiteral where
  grammar =
    label "IP-literal" $
      Grammar
        { render = \x → do
            y ← case x of
              IpLiteral_V6 x → render ipv6AddressGrammar x
              IpLiteral_Future x → render grammar x
            Just $ renderConcat [renderConst "[", y, renderConst "]"]
        , parser =
            P.single (char '[')
              *> asum @[]
                [ IpLiteral_V6 <$> parser ipv6AddressGrammar
                , IpLiteral_Future <$> parser grammar
                ]
              <* P.single (char ']')
        , generator =
            QC.oneof
              [ IpLiteral_V6 <$> generator ipv6AddressGrammar
              , IpLiteral_Future <$> generator grammar
              ]
        }

instance HasGrammar IpvFuture where
  grammar =
    label
      "IPvFuture"
      Grammar
        { render = \IpvFuture {version, address} → do
            rv ← render vg version
            Just $
              renderConcat
                [ renderConst "v"
                , rv
                , renderConst "."
                , renderConst $ BSB.byteString address
                ]
        , parser = do
            P.single $ char 'v'
            version ← parser vg
            P.single $ char '.'
            address ←
              fmap fst $
                P.match $
                  P.many $
                    asum @[]
                      [ void $ unreservedGrammar.parser
                      , void $ subDelimGrammar.parser
                      , void $ P.single $ char ':'
                      ]
            pure IpvFuture {version, address}
        , generator = do
            version ← generator vg
            address ←
              fmap (build . fold) $
                QC.listOf1 $
                  BSB.word8
                    <$> QC.oneof
                      [ generator unreservedGrammar
                      , generator subDelimGrammar
                      , pure $ char ':'
                      ]
            pure IpvFuture {version, address}
        }
   where
    vg = hexdigGrammar UpperCase

-- | Parsing is much more lenient than the spec out of laziness, could be improved
ipv6AddressGrammar ∷ Grammar ByteString
ipv6AddressGrammar =
  label
    "IPv6address"
    Grammar
      { render = Just . renderConst . BSB.byteString
      , parser =
          fmap fst $
            P.match $
              some $
                asum @[]
                  [ void $ parser hex
                  , void $ P.single $ char ':'
                  ]
      , generator =
          fmap build $
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
  h16, ls32 ∷ Gen Builder
  h16 = do
    n ← QC.choose (1, 4)
    fmap fold $ QC.vectorOf n $ BSB.word8 <$> generator hex
  ls32 =
    QC.oneof
      [ h16 ^ pure ":" ^ h16
      , renderGenerator ipv4AddressGrammar
      ]
  hex = hexdigGrammar UpperCase

ipv4AddressGrammar ∷ Grammar ByteString
ipv4AddressGrammar =
  label
    "IPv4address"
    Grammar
      { render = Just . renderConst . BSB.byteString
      , parser = fmap fst $ P.match do
          let o = decOctetGrammar.parser
              d = P.single $ char '.'
          o *> d *> o *> d *> o *> d *> o
      , generator =
          fmap (build . fold . List.intersperse ".") $
            replicateM 4 (renderGenerator decOctetGrammar)
      }

decOctetGrammar ∷ Grammar Word8
decOctetGrammar =
  label
    "dec-octet"
    Grammar
      { render = Just . renderConst . BSB.word8Dec
      , parser = do
          xs ← some digitGrammar.parser
          let n ∷ Natural = foldl' (\t x → (t * 10) + fromIntegral x) 0 xs
          maybe empty pure $ toIntegralSized n
      , generator = arbitrary
      }

regNameGrammar ∷ Grammar ByteString
regNameGrammar =
  label
    "reg-name"
    Grammar
      { render = \x →
          fmap renderConcat
            $ traverse
              ( renderChoices
                  [ render unreservedGrammar
                  , render subDelimGrammar
                  , render pctEncodedGrammar
                  ]
              )
            $ BS.unpack x
      , parser =
          fmap (build . fold) $
            P.many $
              asum @[]
                [ fmap BSB.word8 $ parser unreservedGrammar
                , fmap BSB.word8 $ parser subDelimGrammar
                , fmap BSB.word8 $ parser pctEncodedGrammar
                ]
      , generator =
          fmap (build . fold) $
            QC.listOf $
              QC.oneof
                [ renderGenerator unreservedGrammar
                , renderGenerator subDelimGrammar
                , renderGenerator pctEncodedGrammar
                ]
      }

deriving via
  TheGrammar Host
  instance
    Arbitrary Host

deriving via
  TheGrammar IpLiteral
  instance
    Arbitrary IpLiteral

deriving via
  TheGrammar IpvFuture
  instance
    Arbitrary IpvFuture

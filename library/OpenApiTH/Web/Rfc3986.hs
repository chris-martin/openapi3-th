-- | Very straightforward translation of ABNF from
--   <https://www.ietf.org/rfc/rfc3986.txt>
module OpenApiTH.Web.Rfc3986 where

import Essentials

import Control.Applicative (Alternative (..), asum, liftA2)
import Control.Monad (mfilter, replicateM, replicateM_, unless)
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
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Show (show)
import Prelude (fromIntegral)

import OpenApiTH.Grammar
import OpenApiTH.Web.Rfc2234

data Uri = Uri
  { scheme ∷ Scheme
  , hierPart ∷ HierPart
  , query ∷ Maybe Query
  , fragment ∷ Maybe Fragment
  }
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary Uri

instance Grammar Uri where
  parser = P.label "URI" do
    scheme ← parser
    P.single ':'
    hierPart ← parser
    query ← P.optional $ P.single '?' *> parser
    fragment ← P.optional $ P.single '#' *> parser
    pure Uri {scheme, hierPart, query, fragment}

data HierPart
  = HierPart_Authority Authority PathAbempty
  | HierPart_Absolute PathAbsolute
  | HierPart_Rootless PathRootless
  | HierPart_Empty PathEmpty
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary HierPart

instance Grammar HierPart where
  parser =
    P.label "hier-part" $
      asum @[]
        [ P.chunk "//" *> (HierPart_Authority <$> parser <*> parser)
        , HierPart_Absolute <$> parser
        , HierPart_Rootless <$> parser
        , HierPart_Empty <$> parser
        ]

data UriReference
  = UriReference_Uri Uri
  | UriReference_RelativeRef RelativeRef
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary UriReference

instance Grammar UriReference where
  parser =
    P.label "URI-reference" $
      asum @[]
        [ UriReference_Uri <$> parser
        , UriReference_RelativeRef <$> parser
        ]

data AbsoluteUri = AbsoluteUri
  { scheme ∷ Scheme
  , hierPart ∷ HierPart
  , query ∷ Maybe Query
  }
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary AbsoluteUri

instance Grammar AbsoluteUri where
  parser = P.label "absolute-URI" do
    scheme ← parser
    P.single ':'
    hierPart ← parser
    query ← P.optional $ P.single '?' *> parser
    pure AbsoluteUri {scheme, hierPart, query}

data RelativeRef = RelativeRef
  { relativePart ∷ RelativePart
  , query ∷ Maybe Query
  , fragment ∷ Maybe Fragment
  }
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary RelativeRef

instance Grammar RelativeRef where
  parser = P.label "relative-ref" do
    relativePart ← parser
    query ← P.optional $ P.single '?' *> parser
    fragment ← P.optional $ P.single '#' *> parser
    pure RelativeRef {relativePart, query, fragment}

data RelativePart
  = RelativePart_Authority Authority PathAbempty
  | RelativePart_Absolute PathAbsolute
  | RelativePart_Noscheme PathNoscheme
  | RelativePart_Empty PathEmpty
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary RelativePart

instance Grammar RelativePart where
  parser =
    P.label "relative-part" $
      asum @[]
        [ P.chunk "//" *> (RelativePart_Authority <$> parser <*> parser)
        , RelativePart_Absolute <$> parser
        , RelativePart_Noscheme <$> parser
        , RelativePart_Empty <$> parser
        ]

newtype Scheme = SchemeUnsafe Text

instance Grammar Scheme where
  parser = P.label "scheme" $ fmap (SchemeUnsafe . fst) $ P.match do
    parser @Alpha
    P.takeWhileP Nothing $ isAlpha `or` isDigit `or` inCharset "+-."

instance Arbitrary Scheme where
  arbitrary =
    fmap (SchemeUnsafe . Text.pack) $
      (:)
        <$> alphaGen
        <*> QC.listOf (QC.oneof [alphaGen, digitGen, QC.elements "+-."])

data Authority = Authority
  { userinfo ∷ Maybe Userinfo
  , host ∷ Host
  , port ∷ Maybe Port
  }
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary Authority

instance Grammar Authority where
  parser = P.label "authority" do
    userinfo ← P.optional $ P.try $ parser <* P.single '@'
    host ← parser
    port ← P.optional $ P.single ':' *> parser
    pure Authority {userinfo, host, port}

newtype Userinfo = UserinfoUnsafe Text

instance Grammar Userinfo where
  parser = P.label "userinfo" $ fmap (UserinfoUnsafe . fst) $ P.match do
    P.many $
      asum @[]
        [ void unreservedParser
        , void pctEncodedParser
        , void subDelimsParser
        , void $ P.single ':'
        ]

instance Arbitrary Userinfo where
  arbitrary =
    fmap (UserinfoUnsafe . Text.concat) $
      QC.listOf $
        QC.oneof
          [ Text.singleton <$> unreservedGen
          , pctEncodedGen
          , Text.singleton <$> subDelimsGen
          , pure ":"
          ]

data Host
  = Host_IpLiteral IpLiteral
  | Host_Ipv4 Ipv4Address
  | Host_RegName RegName
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary Host

instance Grammar Host where
  parser =
    P.label "host" $
      asum @[]
        [ Host_IpLiteral <$> parser
        , Host_Ipv4 <$> parser
        , Host_RegName <$> parser
        ]

newtype Port = PortUnsafe Text

instance Grammar Port where
  parser = P.label "port" $ fmap (PortUnsafe . fst) $ P.match $ P.many digitParser

instance Arbitrary Port where
  arbitrary = PortUnsafe . Text.pack <$> QC.listOf digitGen

data IpLiteral
  = IpLiteral_V6 Ipv6Address
  | IpLiteral_Future IpvFuture
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary IpLiteral

instance Grammar IpLiteral where
  parser =
    P.label "IP-literal" $
      P.single '['
        *> asum @[]
          [ IpLiteral_V6 <$> parser
          , IpLiteral_Future <$> parser
          ]
        <* P.single ']'

newtype IpvFuture = IpvFutureUnsafe Text

instance Grammar IpvFuture where
  parser = P.label "IPvFuture" $ fmap (IpvFutureUnsafe . fst) $ P.match do
    P.single 'v'
    hexdigParser
    P.single '.'
    P.takeWhile1P Nothing $ isUnreserved `or` isSubDelims `or` (== ':')

instance Arbitrary IpvFuture where
  arbitrary = do
    x ← hexdigGen
    xs ← QC.listOf1 $ QC.oneof [unreservedGen, subDelimsGen, pure ':']
    pure $ IpvFutureUnsafe $ Text.pack $ ['v', x, '.'] <> xs

newtype Ipv6Address = Ipv6AddressUnsafe Text

instance Grammar Ipv6Address

instance Arbitrary Ipv6Address

newtype Ipv4Address = Ipv4AddressUnsafe Text

instance Grammar Ipv4Address

instance Arbitrary Ipv4Address

newtype RegName = RegNameUnsafe Text

instance Grammar RegName

instance Arbitrary RegName

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

newtype Segment = SegmentUnsafe Text

instance Grammar Segment

instance Arbitrary Segment

newtype SegmentNz = SegmentNzUnsafe Text

instance Grammar SegmentNz

instance Arbitrary SegmentNz

newtype SegmentNzNc = SegmentNzNcUnsafe Text

instance Grammar SegmentNzNc

instance Arbitrary SegmentNzNc

newtype Query = QueryUnsafe Text

instance Grammar Query

instance Arbitrary Query

newtype Fragment = FragmentUnsafe Text

instance Grammar Fragment

instance Arbitrary Fragment

newtype PctEncoded = PctEncoded Text

instance Grammar PctEncoded where
  parser = P.label "pct-encoded" $ fmap (PctEncoded . fst) $ P.match $ do
    P.single '%'
    replicateM_ 2 $ parser @Hexdig

instance Arbitrary PctEncoded where
  arbitrary =
    fmap (PctEncoded . Text.pack . ('%' :)) $
      replicateM 2 $
        fmap (\(HexdigUnsafe x) → x) $
          arbitrary @Hexdig

newtype Unreserved = UnreservedUnsafe Char

instance Grammar Unreserved where
  parser =
    P.label "unreserved" $
      fmap UnreservedUnsafe $
        asum @[]
          [ (\(AlphaUnsafe x) → x) <$> parser
          , (\(DigitUnsafe x) → x) <$> parser
          , P.satisfy "-._~"
          ]

instance Arbitrary Unreserved where
  arbitrary =
    fmap UnreservedUnsafe $
      QC.oneof
        [ (\(AlphaUnsafe x) → x) <$> arbitrary
        , (\(DigitUnsafe x) → x) <$> arbitrary
        , QC.elements "-._~"
        ]

newtype Reserved = ReservedUnsafe Char
 deriving IsChar via CoercedChar Reserved
 deriving Grammar via Named "reserved" (Tested Reserved)

instance Testable Reserved where
  charIs x = charIs @GenDelim x || charIs @SubDelim x

instance Arbitrary Reserved where
  arbitrary =
    fmap ReservedUnsafe $
      QC.oneof
        [ (\(GenDelimUnsafe x) → x) <$> arbitrary
        , (\(SubDelimUnsafe x) → x) <$> arbitrary
        ]

newtype GenDelim = GenDelimUnsafe Char
  deriving Arbitrary via Enumerated GenDelim
  deriving Grammar via Named "gen-delims" GenDelim
  deriving IsChar via CoercedChar GenDelim

instance Enumerable GenDelim where
  enumerate = ":/?#[]@"

newtype SubDelim = SubDelimUnsafe Char
  deriving Arbitrary via Enumerated SubDelim
  deriving Grammar via Named "sub-delims" SubDelim
  deriving IsChar via CoercedChar SubDelim


instance Enumerable SubDelim where
  enumerate = "!$&'()*+,;="

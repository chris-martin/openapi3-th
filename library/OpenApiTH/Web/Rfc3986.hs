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
import Prelude (fromIntegral, (+))

import Data.Bits (shiftL, shiftR, (.&.))
import Data.Foldable (fold)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
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
    P.many $
      asum @[]
        [ void $ parser @Alpha
        , void $ parser @DigitChar
        , void $ parser @SchemeSymbol
        ]

instance Arbitrary Scheme where
  arbitrary =
    fmap (SchemeUnsafe . Text.pack) $
      (:)
        <$> ((.char) <$> arbitrary @Alpha)
        <*> QC.listOf
          ( QC.oneof
              [ (.char) <$> arbitrary @Alpha
              , (.char) <$> arbitrary @DigitChar
              , (.char) <$> arbitrary @SchemeSymbol
              ]
          )

newtype SchemeSymbol = SchemeSymbolUnsafe {char ∷ Char}
  deriving Arbitrary via Enumerated SchemeSymbol
  deriving Grammar via Enumerated SchemeSymbol
  deriving IsChar via CoercedChar SchemeSymbol

instance Enumerable SchemeSymbol where
  enumerate = "+-."

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
        [ void $ parser @Unreserved
        , void $ parser @PctEncoded
        , void $ parser @SubDelim
        , void $ P.single ':'
        ]

instance Arbitrary Userinfo where
  arbitrary =
    fmap (UserinfoUnsafe . TL.toStrict . TB.toLazyText . fold) $
      QC.listOf $
        QC.oneof
          [ render <$> arbitrary @Unreserved
          , render <$> arbitrary @PctEncoded
          , render <$> arbitrary @SubDelim
          , pure ":"
          ]

data Host
  = Host_IpLiteral IpLiteral
  | Host_Ipv4 Ipv4Address
  | Host_RegName RegName
  deriving stock Generic
  deriving Arbitrary via GenericArbitrary Host

instance Grammar Host where
  render = \case
    Host_IpLiteral x → render x
    Host_Ipv4 x → render x
    Host_RegName x → render x
  parser =
    P.label "host" $
      asum @[]
        [ Host_IpLiteral <$> parser
        , Host_Ipv4 <$> parser
        , Host_RegName <$> parser
        ]

newtype Port = PortUnsafe Text

instance Grammar Port where
  parser =
    P.label "port" $
      fmap (PortUnsafe . fst) $
        P.match $
          P.many $
            parser @DigitChar

instance Arbitrary Port where
  arbitrary =
    fmap (PortUnsafe . Text.pack) $
      QC.listOf $
        fmap (.char) $
          arbitrary @DigitChar

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
    parser @Hexdig
    P.single '.'
    P.many $
      asum @[]
        [ void $ parser @Unreserved
        , void $ parser @SubDelim
        , void $ P.single ':'
        ]

instance Arbitrary IpvFuture where
  arbitrary = do
    x ← hexdigChar <$> arbitrary @Hexdig
    xs ←
      QC.listOf1 $
        QC.oneof
          [ (.char) <$> arbitrary @Unreserved
          , (.char) <$> arbitrary @SubDelim
          , pure ':'
          ]
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

newtype PctEncoded = PctEncoded {byte ∷ Word8}
  deriving newtype Arbitrary

instance Grammar PctEncoded where
  render x =
    TB.singleton '%'
      <> render (HexdigUnsafe $ x.byte `shiftR` 4)
      <> render (HexdigUnsafe $ x.byte .&. 15)
  parser = P.label "pct-encoded" $ do
    P.single '%'
    HexdigUnsafe a ← parser
    HexdigUnsafe b ← parser
    pure $ PctEncoded $ (a `shiftL` 4) + b

newtype Unreserved = UnreservedUnsafe {char ∷ Char}

instance Grammar Unreserved where
  render = TB.singleton . (.char)
  parser =
    P.label "unreserved" $
      fmap UnreservedUnsafe $
        asum @[]
          [ (.char) <$> parser @Alpha
          , (.char) <$> parser @DigitChar
          , (.char) <$> parser @UnreservedSymbol
          ]

instance Arbitrary Unreserved where
  arbitrary =
    fmap UnreservedUnsafe $
      QC.oneof
        [ (.char) <$> arbitrary @Alpha
        , (.char) <$> arbitrary @DigitChar
        , (.char) <$> arbitrary @UnreservedSymbol
        ]

newtype UnreservedSymbol = UnreservedSymbolUnsafe {char ∷ Char}
  deriving (Arbitrary, Grammar) via Enumerated UnreservedSymbol
  deriving IsChar via CoercedChar UnreservedSymbol

instance Enumerable UnreservedSymbol where
  enumerate = "-._~"

newtype Reserved = ReservedUnsafe {char ∷ Char}
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

newtype GenDelim = GenDelimUnsafe {char ∷ Char}
  deriving (Arbitrary, Testable) via Enumerated GenDelim
  deriving Grammar via Named "gen-delims" GenDelim
  deriving IsChar via CoercedChar GenDelim

instance Enumerable GenDelim where
  enumerate = ":/?#[]@"

newtype SubDelim = SubDelimUnsafe {char ∷ Char}
  deriving (Arbitrary, Testable) via Enumerated SubDelim
  deriving Grammar via Named "sub-delims" SubDelim
  deriving IsChar via CoercedChar SubDelim

instance Enumerable SubDelim where
  enumerate = "!$&'()*+,;="

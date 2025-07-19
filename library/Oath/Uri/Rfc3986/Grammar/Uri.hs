module Oath.Uri.Rfc3986.Grammar.Uri where

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
import Optics
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
import Oath.Uri.Rfc3986.Grammar.Appendages
import Oath.Uri.Rfc3986.Grammar.Authority
import Oath.Uri.Rfc3986.Grammar.Characters
import Oath.Uri.Rfc3986.Grammar.Host
import Oath.Uri.Rfc3986.Grammar.Path
import Oath.Uri.Rfc3986.Grammar.RelativeRef
import Oath.Uri.Rfc3986.Grammar.Scheme
import Oath.Uri.Rfc3986.Grammar.Segment

data Uri = Uri
  { scheme ∷ ByteString
  , hierPart ∷ HierPart
  , query ∷ Maybe ByteString
  , fragment ∷ Maybe ByteString
  }

-- | 'Uri' without a fragment
--
-- <https://www.rfc-editor.org/rfc/rfc3986#section-4.3>
data AbsoluteUri = AbsoluteUri
  { scheme ∷ ByteString
  , hierPart ∷ HierPart
  , query ∷ Maybe ByteString
  }

data HierPart
  = HierPart_Authority Authority (Seq ByteString)
  | HierPart_Absolute (Seq ByteString)
  | HierPart_Rootless (NESeq ByteString)
  | HierPart_Empty

makeFieldLabels ''Uri
makeFieldLabels ''AbsoluteUri
makePrismLabels ''HierPart

instance HasGrammar Uri where
  grammar =
    label "URI"
      $ isoGrammar
        ( iso
            ( \Uri {scheme, hierPart, query, fragment} →
                scheme :& hierPart :& query :& fragment
            )
            ( \(scheme :& hierPart :& query :& fragment) →
                Uri {scheme, hierPart, query, fragment}
            )
        )
      $ (schemeGrammar <+ constGrammar ":")
        <+> grammar
        <+> optionalGrammar (constGrammar "?" +> queryGrammar)
        <+> optionalGrammar (constGrammar "#" +> fragmentGrammar)

instance HasGrammar AbsoluteUri where
  grammar =
    label "absolute-uri"
      $ isoGrammar
        ( iso
            ( \AbsoluteUri {scheme, hierPart, query} →
                scheme :& hierPart :& query
            )
            ( \(scheme :& hierPart :& query) →
                AbsoluteUri {scheme, hierPart, query}
            )
        )
      $ (schemeGrammar <+ constGrammar ":")
        <+> grammar
        <+> optionalGrammar (constGrammar "?" +> queryGrammar)

instance HasGrammar HierPart where
  grammar =
    label "hier-part" $
      grammarAlternatives
        [ prismGrammar
            ( #_HierPart_Authority
                % iso
                  (\(authority, path) → authority :& path)
                  (\(authority :& path) → (authority, path))
            )
            $ grammar <+> pathAbemptyGrammar
        , prismGrammar #_HierPart_Absolute pathAbsoluteGrammar
        , prismGrammar #_HierPart_Rootless pathRootlessGrammar
        , prismGrammar #_HierPart_Empty pathEmptyGrammar
        ]

deriving via
  TheGrammar Uri
  instance
    Arbitrary Uri

deriving via
  TheGrammar AbsoluteUri
  instance
    Arbitrary AbsoluteUri

deriving via
  TheGrammar HierPart
  instance
    Arbitrary HierPart

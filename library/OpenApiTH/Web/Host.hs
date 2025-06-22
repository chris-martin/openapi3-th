module OpenApiTH.Web.Host where

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
import Optics.TH
import Test.QuickCheck (Gen)
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary.Generic
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Show (show)
import Prelude (fromIntegral)

hostP ∷ P.Parsec Void Text Text
hostP = ipv6P <|> ipv4P <|> regNameP

hostG ∷ Gen Text
hostG = QC.oneof [regNameG, ipv4G, ipv6G]

ipv6P ∷ P.Parsec Void Text Text
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

ipv4P ∷ P.Parsec Void Text Text
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

regNameP ∷ P.Parsec Void Text Text
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

module OpenApiTH.PercentEncoding where

import Essentials

import Control.Applicative ((<|>))
import Data.ByteString (StrictByteString)
import Data.ByteString.Builder qualified as BSB
import Data.Char qualified as Char
import Data.Either (either)
import Data.Foldable (fold)
import Data.Function (const)
import Data.Text (StrictText)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Encoding qualified as TL
import Data.Word (Word8)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Byte qualified as P
import Prelude (fromIntegral, (*), (+))

type Parser = P.Parsec Void StrictByteString

percentDecodeUtf8 ∷ StrictByteString → Maybe StrictText
percentDecodeUtf8 x =
  (BSB.toLazyByteString <$> P.parseMaybe percentEncodedP x)
    >>= (either (const Nothing) (Just . TL.toStrict) . TL.decodeUtf8')

percentEncodedP ∷ Parser BSB.Builder
percentEncodedP =
  fmap fold $
    P.many $
      (BSB.word8 <$> escapeP)
        <|> (BSB.byteString <$> P.takeWhile1P (Just "unescaped byte") (/= percent))

percent ∷ Word8
percent = fromIntegral $ Char.ord '%'

escapeP ∷ Parser Word8
escapeP =
  P.single percent
    *> ( do
          a ← P.hexDigitChar
          b ← P.hexDigitChar
          pure $ a * 16 + b
       )

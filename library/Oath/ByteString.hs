module Oath.ByteString where

import Essentials

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int)

buildStrict ∷ Builder → ByteString
buildStrict = BSL.toStrict . BSB.toLazyByteString

intDecBs ∷ Int → ByteString
intDecBs = buildStrict . BSB.intDec

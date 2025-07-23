module Oath.ByteString where

import Essentials

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL

buildStrict ∷ Builder → ByteString
buildStrict = BSL.toStrict . BSB.toLazyByteString

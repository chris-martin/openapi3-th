-- | <https://www.rfc-editor.org/rfc/rfc7617>
module Oath.BasicAuth where

import Essentials

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL
import Data.Either (either)
import Data.Word8 qualified as Word8

readBasicAuthentication ∷ ByteString → Maybe ByteString
readBasicAuthentication bs = do
  let (a, b) = BS.splitAt 6 bs
  guard $ BS.map Word8.toLower a == "basic "
  either (\_ → Nothing) Just $ decodeBase64Untyped b

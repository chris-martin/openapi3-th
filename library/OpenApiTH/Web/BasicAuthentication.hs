-- | <https://www.rfc-editor.org/rfc/rfc7617>
module OpenApiTH.Web.BasicAuthentication where

import Essentials

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either (either)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Base64 qualified as Base64
import Data.Word8 qualified as Word8

readBasicAuthentication ∷ ByteString → Maybe Text
readBasicAuthentication bs =
  let (a, b) = BS.splitAt 6 bs
   in if BS.map Word8.toLower a == "basic "
        then
          either (\_ → Nothing) Just $
            Base64.decodeBase64UntypedWith Text.decodeUtf8' b
        else Nothing

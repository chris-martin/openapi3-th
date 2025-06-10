module OpenApiTH.Operation.OutgoingResponse where

import Essentials

import Conduit
import Control.Monad.Fail
import Control.Monad.Yield
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (LazyByteString)
import System.IO (IO)

import Data.ByteString qualified as BS
import Data.Word (Word16)

-- | Intermediate representation of a response header to be turned into
--   an HTTP message
data OutgoingResponse = OutgoingResponse
  { statusCode ∷ ByteString
  , contentType ∷ ByteString
  }
  deriving stock (Eq, Show)

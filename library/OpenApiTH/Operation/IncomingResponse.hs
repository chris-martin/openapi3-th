module OpenApiTH.Operation.IncomingResponse where

import Essentials

import Data.ByteString (ByteString)

-- | Intermediate representation of a response header read from
--   an HTTP message
data IncomingResponse = IncomingResponse
  { statusCode ∷ ByteString
  , contentType ∷ Maybe ByteString
  }
  deriving stock (Eq, Show)

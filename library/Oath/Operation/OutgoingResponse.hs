module Oath.Operation.OutgoingResponse where

import Essentials

import Data.ByteString (ByteString)

-- | Intermediate representation of a response header to be turned into
--   an HTTP message
data OutgoingResponse = OutgoingResponse
  { statusCode ∷ ByteString
  , contentType ∷ ByteString
  }
  deriving stock (Eq, Show)

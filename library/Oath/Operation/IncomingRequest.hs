module Oath.Operation.IncomingRequest where

import Essentials

import Data.ByteString (ByteString)
import Data.Sequence (Seq (..))

-- | Intermediate representation of a request header read
--   from an HTTP message
data IncomingRequest = IncomingRequest
  { host ∷ Maybe ByteString
  , basicAuthentication ∷ Maybe ByteString
  , method ∷ ByteString
  , path ∷ Seq ByteString
  , query ∷ [(ByteString, Maybe ByteString)]
  }
  deriving stock (Eq, Show)

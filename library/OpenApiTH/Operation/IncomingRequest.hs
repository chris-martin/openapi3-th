module OpenApiTH.Operation.IncomingRequest where

import Essentials

import Data.ByteString (ByteString)
import Iri.Data (Host, Path, Port)

-- | Intermediate representation of a request header read
--   from an HTTP message
data IncomingRequest = IncomingRequest
  { server ∷ Maybe (Host, Port)
  , method ∷ ByteString
  , path ∷ Path
  , query ∷ [(ByteString, Maybe ByteString)]
  }
  deriving stock (Eq, Show)

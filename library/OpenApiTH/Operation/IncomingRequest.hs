module OpenApiTH.Operation.IncomingRequest where

import Essentials

import Data.ByteString (ByteString)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Numeric.Natural (Natural)

import OpenApiTH.OpenApi

-- | Intermediate representation of a request header read
--   from an HTTP message
data IncomingRequest = IncomingRequest
  { host ∷ Text
  , authorization ∷ Maybe Text
  , method ∷ ByteString
  , path ∷ Seq Text
  , query ∷ [(ByteString, Maybe ByteString)]
  }
  deriving stock (Eq, Show)

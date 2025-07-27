module Oath.Operation.OutgoingRequest where

import Essentials

import Data.ByteString (ByteString)
import Data.Sequence (Seq)
import Optics

-- | Intermediate representation of a request header to be turned into
--   an HTTP message
data OutgoingRequest = OutgoingRequest
  { scheme ∷ ByteString
  , path ∷ Seq ByteString
  , method ∷ ByteString
  , query ∷ [(ByteString, Maybe ByteString)]
  , accept ∷ ByteString
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''OutgoingRequest

module Oath.Operation.OutgoingRequest where

import Essentials

import Data.ByteString (ByteString)
import Optics

import Oath.Uri.Uri (Uri)

-- | Intermediate representation of a request header to be turned into
--   an HTTP message
data OutgoingRequest = OutgoingRequest
  { location ∷ Uri
  , method ∷ ByteString
  , query ∷ [(ByteString, Maybe ByteString)]
  , accept ∷ ByteString
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''OutgoingRequest

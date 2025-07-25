module Oath.Operation.OutgoingRequest where

import Essentials

import Data.ByteString (ByteString)
import Data.Sequence (Seq)
import Optics

import Oath.Uri

-- | Intermediate representation of a request header to be turned into
--   an HTTP message
data OutgoingRequest = OutgoingRequest
  { scheme ∷ ByteString
  , hierPart ∷ HierPart
  , method ∷ ByteString
  , query ∷ [(ByteString, Maybe ByteString)]
  , accept ∷ ByteString
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''OutgoingRequest

instance LabelOptic "authority" An_AffineTraversal OutgoingRequest OutgoingRequest Authority Authority where
  labelOptic = #hierPart % #authority

instance LabelOptic "path" A_Lens OutgoingRequest OutgoingRequest (Seq ByteString) (Seq ByteString) where
  labelOptic = #hierPart % #path

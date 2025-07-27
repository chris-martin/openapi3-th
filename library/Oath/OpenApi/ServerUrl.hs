module Oath.OpenApi.ServerUrl where

import Data.ByteString (ByteString)
import Data.Sequence (Seq)

import Oath.Uri

data ServerUrl = ServerUrl
  { scheme ∷ ByteString
  , authority ∷ Authority
  , path ∷ Seq ByteString
  }

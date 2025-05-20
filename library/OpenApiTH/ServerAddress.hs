module OpenApiTH.ServerAddress where

import Essentials

import Data.ByteString (ByteString)
import Iri.Data (Authority, Fragment, Iri, Path, Query, Security)

data ServerAddress = ServerAddress
  { security ∷ Security
  , authority ∷ Authority
  , path ∷ Path
  }

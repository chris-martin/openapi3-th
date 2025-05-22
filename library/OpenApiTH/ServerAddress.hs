module OpenApiTH.ServerAddress where

import Essentials

import Data.ByteString (ByteString)
import Data.Word
import Iri.Data

data ServerAddress = ServerAddress
  { security ∷ Security
  , authority ∷ Authority
  , path ∷ Path
  }

localhost ∷ ServerAddress
localhost =
  ServerAddress
    { security = Security False
    , authority = Authority MissingUserInfo (NamedHost $ RegName [DomainLabel "localhost"]) MissingPort
    , path = Path []
    }

setServerPort ∷ Word16 → ServerAddress → ServerAddress
setServerPort port x =
  x {authority = Authority userInfo host (PresentPort port)}
 where
  Authority userInfo host _ = x.authority

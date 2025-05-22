module OpenApiTH.ServerAddress where

import Essentials

import Control.Monad.Fail
import Data.ByteString (ByteString)
import Data.Either (Either (..))
import Data.String (IsString)
import Data.Text qualified as Text
import Data.Word
import Iri.Data
import Iri.Parsing.Text qualified as P
import Language.Haskell.TH.Quote

data ServerAddress = ServerAddress
  { security ∷ Security
  , authority ∷ Authority
  , path ∷ Path
  }

serverAddressQQ ∷ QuasiQuoter
serverAddressQQ =
  QuasiQuoter
    { quoteExp = \s → case P.httpIri (Text.pack s) of
        Left e → fail $ Text.unpack e
        Right (HttpIri security host port path query fragment) → _
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

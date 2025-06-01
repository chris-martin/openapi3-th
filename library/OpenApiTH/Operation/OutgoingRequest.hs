module OpenApiTH.Operation.OutgoingRequest where

import Essentials

import Conduit
import Control.Monad.Fail
import Control.Monad.Yield
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.Word (Word16)
import Iri.Data (Authority, Fragment, Host, Iri, Path (..), Port, Query, Scheme, Security (..))
import System.IO (IO)

import OpenApiTH.OpenApi.Server
import OpenApiTH.OpenApi.ServerUrl

-- | Intermediate representation of a request header to be turned into
--   an HTTP message
data OutgoingRequest = OutgoingRequest
  { server ∷ Maybe Server
  , method ∷ ByteString
  , path ∷ Path
  , query ∷ [(ByteString, Maybe ByteString)]
  , accept ∷ ByteString
  }
  deriving stock (Eq, Show)

setRequestBuilderServerUrl ∷ ∀ op. ServerUrl → OutgoingRequest → OutgoingRequest
setRequestBuilderServerUrl
  ServerUrl {security, host, port, path = Path pathPrefix}
  rb@OutgoingRequest {path = Path path} =
    rb
      { server = Just Server {security, host, port}
      , path = Path $ pathPrefix <> path
      }

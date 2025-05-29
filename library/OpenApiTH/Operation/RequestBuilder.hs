module OpenApiTH.Operation.RequestBuilder where

import Essentials

import Conduit
import Control.Monad.Fail
import Control.Monad.Yield
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Iri.Data (Authority, Fragment, Host, Iri, Path (..), Port, Query, Scheme, Security (..))
import System.IO (IO)

import Data.ByteString qualified as BS
import Data.Word (Word16)
import OpenApiTH.OpenApi.Server

-- | Intermediate representation of a request header to be turned into
--   an HTTP message
data RequestBuilder = RequestBuilder
  { server ∷ Maybe Server
  , method ∷ ByteString
  , path ∷ Path
  , query ∷ [(ByteString, Maybe ByteString)]
  }
  deriving stock (Eq, Show)

setRequestBuilderServerUrl ∷ ∀ op. ServerUrl → RequestBuilder → RequestBuilder
setRequestBuilderServerUrl
  ServerUrl {security, host, port, path = Path pathPrefix}
  rb@RequestBuilder {path = Path path} =
    rb
      { server = Just Server {security, host, port}
      , path = Path $ pathPrefix <> path
      }

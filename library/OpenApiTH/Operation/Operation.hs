module OpenApiTH.Operation.Operation where

import Essentials

import Conduit
import Control.Monad.Fail
import Control.Monad.Yield
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (LazyByteString)
import Iri.Data (Authority, Fragment, Host, Iri, Path (..), Port, Query, Scheme, Security (..))
import System.IO (IO)

import Data.Word (Word16)
import OpenApiTH.OpenApi.Server

type family OperationRequest op ∷ Type

type family OperationResponse op ∷ Type

type OperationServer op m = OperationRequest op → m (OperationResponse op)

class Operation op where
  buildOperationRequest ∷ OperationRequest op → IO RequestBuilder
  buildOperationResponse ∷ OperationResponse op → IO ResponseBuilder
  readOperationRequest ∷ RequestReader → IO (OperationRequest op)
  readOperationResponse ∷ ResponseReader → IO (OperationResponse op)

data RequestBuilder = RequestBuilder
  { server ∷ Maybe Server
  , method ∷ ByteString
  , path ∷ Path
  , query ∷ [(ByteString, Maybe ByteString)]
  , body ∷ LazyByteString
  }

data ResponseBuilder = ResponseBuilder

data RequestReader

data ResponseReader

setRequestBuilderServerUrl ∷ ∀ op. ServerUrl → RequestBuilder → RequestBuilder
setRequestBuilderServerUrl
  ServerUrl {security, host, port, path = Path pathPrefix}
  rb@RequestBuilder {path = Path path} =
    rb
      { server = Just Server {security, host, port}
      , path = Path $ pathPrefix <> path
      }

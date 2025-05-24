-- | Integration with the @http-client@ library
module OpenApiTH.HttpClient where

import Essentials

import Conduit
import Control.Monad.Fail
import Control.Monad.Yield
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (LazyByteString)
import Network.HTTP.Simple
import Network.Wai.Handler.Warp
import System.IO (IO)
import Test.Hspec
import Prelude (fromIntegral)

import OpenApiTH.Operation
import OpenApiTH.ServerAddress
import OpenApiTH.Wai

class HttpClientOperation op where
  operationRequestToHttpClient ∷ OperationRequest op → IO Request
  httpClientToOperationResponse
    ∷ Response (ConduitT i ByteString IO ()) → IO (OperationResponse op)

setHttpClientRequestServerAddress ∷ ∀ op. ServerAddress → Request → Request
setHttpClientRequestServerAddress = _

httpClientOperationResponse
  ∷ ∀ op m
   . MonadFail m
  ⇒ Response LazyByteString
  → m (OperationResponse op)
httpClientOperationResponse = _

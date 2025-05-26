module OpenApiTH.Operation.Testing where

import Essentials

import Control.Monad.Fail
import Control.Monad.Yield
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (LazyByteString)
import Network.HTTP.Simple
import Network.Wai.Handler.Warp
import System.IO (IO)
import Test.Hspec
import Prelude (fromIntegral)

import OpenApiTH.OpenApi.Server
import OpenApiTH.Operation.HttpClient
import OpenApiTH.Operation.Operation
import OpenApiTH.Operation.Wai

-- | Test making an HTTP request using http-client as the client
--   and Warp as the server
assertHttpClientWarpExchange
  ∷ ∀ op
   . (HttpClientOperation op, WaiOperation op)
  ⇒ (Show (OperationResponse op), Eq (OperationResponse op))
  ⇒ OperationRequest op
  → OperationServer op IO
  → Expectation
assertHttpClientWarpExchange request server = do
  expectedResponse ← server request
  testWithApplication (pure $ operationWaiApplication @op server) \port → do
    let serverUrl = localhost & setServerPort (fromIntegral port)
    httpClientRequest ← setHttpClientRequestServerUrl serverUrl <$> operationRequestToHttpClient @op request
    withResponse httpClientRequest $ \httpClientResponse → do
      response ← httpClientToOperationResponse @op httpClientResponse
      response `shouldBe` expectedResponse

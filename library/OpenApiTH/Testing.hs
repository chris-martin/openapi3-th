module OpenApiTH.Testing where

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

import OpenApiTH.HttpClient
import OpenApiTH.Operation
import OpenApiTH.ServerAddress
import OpenApiTH.Wai

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
    let serverAddress = localhost & setServerPort (fromIntegral port)
    httpClientRequest ← operationRequestToHttpClient @op request
    httpClientResponse ←
      httpLBS $
        setHttpClientRequestServerAddress serverAddress httpClientRequest
    response ← httpClientOperationResponse @op httpClientResponse
    response `shouldBe` expectedResponse

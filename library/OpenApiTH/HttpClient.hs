-- | Integration with the @http-client@ library
module OpenApiTH.HttpClient where

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

import OpenApiTH.Operation
import OpenApiTH.ServerAddress
import OpenApiTH.Wai

operationRequestHttpClient ∷ ∀ op. ServerAddress → OperationRequest op → Request
operationRequestHttpClient = _

httpClientOperationResponse
  ∷ ∀ op m
   . MonadFail m
  ⇒ Response LazyByteString
  → m (OperationResponse op)
httpClientOperationResponse = _

assertHttpClientExchange
  ∷ ∀ op
   . (Show (OperationResponse op), Eq (OperationResponse op))
  ⇒ OperationRequest op
  → OperationServer op IO
  → Expectation
assertHttpClientExchange request server = do
  expectedResponse ← server request
  testWithApplication (pure $ operationWaiApplication @op server) \port → do
    let serverAddress = localhost & setServerPort (fromIntegral port)
        httpClientRequest = operationRequestHttpClient @op serverAddress request
    httpClientResponse ← httpLBS httpClientRequest
    response ← httpClientOperationResponse @op httpClientResponse
    response `shouldBe` expectedResponse

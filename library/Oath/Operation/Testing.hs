module Oath.Operation.Testing where

import Essentials

import Control.Monad.Fail
import Control.Monad.Yield
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (LazyByteString)
import Network.HTTP.Simple
import Network.Wai.Handler.Warp
import Optics
import System.IO (IO)
import Test.Hspec
import Prelude (fromIntegral)

import Oath.Operation.HttpClient
import Oath.Operation.Message
import Oath.Operation.Operation
import Oath.Operation.OutgoingRequest
import Oath.Operation.Wai

-- | Test making an HTTP request using http-client as the client
--   and Warp as the server
assertHttpClientWarpExchange
  ∷ ∀ op
   . Operation op
  ⇒ (Show (OperationResponse op), Eq (OperationResponse op))
  ⇒ OperationRequest op
  → OperationServer op IO
  → Expectation
assertHttpClientWarpExchange request server = do
  expectedResponse ← server request
  testWithApplication (pure $ operationWaiApplication @op server) \port → do
    let serverUrl = localhostPort $ fromIntegral port
    request ←
      over (#head % #location) (serverUrl <>)
        <$> buildOperationRequest @op request
    httpClientRequest ← buildHttpClientRequest request
    withResponse httpClientRequest $ \httpClientResponse → do
      responseReader ← readHttpClientResponse httpClientResponse
      response ← readOperationResponse @op responseReader
      response `shouldBe` expectedResponse

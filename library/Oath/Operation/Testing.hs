module Oath.Operation.Testing where

import Essentials

import Data.Sequence qualified as Seq
import Network.HTTP.Simple
import Network.Wai.Handler.Warp
import System.IO (IO)
import Test.Hspec

import Oath.ByteString
import Oath.OpenApi
import Oath.Operation.HttpClient
import Oath.Operation.Operation
import Oath.Operation.Wai
import Oath.Uri

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
    let authority =
          Authority
            { userinfo = Nothing
            , host = Host_RegName "localhost"
            , port = Just $ intDecBs port
            }
        serverUrl = ServerUrl {scheme = "http", authority, path = Seq.Empty}
    requestMessage ← buildOperationRequest @op request
    httpClientRequest ← buildHttpClientRequest serverUrl requestMessage
    withResponse httpClientRequest $ \httpClientResponse → do
      responseReader ← readHttpClientResponse httpClientResponse
      response ← readOperationResponse @op responseReader
      response `shouldBe` expectedResponse

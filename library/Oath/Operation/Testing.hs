module Oath.Operation.Testing where

import Essentials

import Data.ByteString.Builder qualified as BSB
import Data.Sequence qualified as Seq
import Network.HTTP.Simple
import Network.Wai.Handler.Warp
import Optics
import System.IO (IO)
import Test.Hspec
import Prelude (fromIntegral)

import Oath.ByteString
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
    let baseUri =
          AbsoluteUri
            { scheme = "http"
            , hierPart =
                HierPart_Authority
                  Authority
                    { userinfo = Nothing
                    , host = Host_RegName "localhost"
                    , port = Just $ buildStrict $ BSB.intDec port
                    }
                  Seq.Empty
            , query = Nothing
            }
    request ←
      over (#head % #location) (resolveUriReference baseUri)
        <$> buildOperationRequest @op request
    httpClientRequest ← buildHttpClientRequest request
    withResponse httpClientRequest $ \httpClientResponse → do
      responseReader ← readHttpClientResponse httpClientResponse
      response ← readOperationResponse @op responseReader
      response `shouldBe` expectedResponse

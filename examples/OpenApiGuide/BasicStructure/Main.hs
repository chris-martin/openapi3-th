module OpenApiGuide.BasicStructure.Main where

import Essentials

import Test.Hspec

import Data.Foldable
import Data.Text (Text)
import Language.Haskell.TH qualified as TH
import Network.HTTP.Simple
import Network.Wai.Handler.Warp
import System.IO (IO)
import Prelude (fromIntegral)

import OpenApiTH

declare $
  specFile "examples/OpenApiGuide/BasicStructure/openapi.yaml"
    <> operation ("get /users" & setOperationName "GetUsers")

server ∷ OperationServer GetUsers IO
server () = pure ["AJ", "Pat"]

spec ∷ Spec
spec = do
  it @(IO ()) "" do
    operationRequestBs @GetUsers [serverAddressQQ|http://api.example.com/v1|] ()
      `shouldBe` fold
        [ "GET /v1/users HTTP/1.1\r\n"
        , "Host: api.example.com\r\n"
        , "Accept: application/json\r\n"
        , "User-Agent: haskell-openapi3-th\r\n"
        , "\r\n"
        ]
  it @(IO ()) "" do
    testWithApplication (pure $ operationWaiApplication @GetUsers server) \port → do
      let serverAddress = localhost & setServerPort (fromIntegral port)
          httpClientRequest = operationRequestHttpClient @GetUsers serverAddress ()
      httpClientResponse ← httpLBS httpClientRequest
      response ← httpClientOperationResponse @GetUsers httpClientResponse
      response `shouldBe` ["AJ", "Pat"]

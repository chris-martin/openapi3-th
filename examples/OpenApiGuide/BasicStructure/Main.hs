module OpenApiGuide.BasicStructure.Main where

import Essentials

import Test.Hspec

import Data.Foldable
import Data.List (map)
import Data.Text (Text)
import Language.Haskell.TH qualified as TH
import Network.HTTP.Simple
import Network.Wai.Handler.Warp
import System.IO (IO)
import Prelude (fromIntegral, show)

import OpenApiTH

declare $
  specFile "examples/OpenApiGuide/BasicStructure/openapi.yaml"
    <> operation ("get /users" & setOperationName "GetUsers")

server ∷ OperationServer GetUsers IO
server () = pure ["AJ", "Pat"]

spec ∷ Spec
spec = do
  it @Expectation "" do
    request ← operationRequestToHttpClient @GetUsers ()
    show request
      `shouldBe` "Request {\n\
                 \  host                 = \"localhost\"\n\
                 \  port                 = 80\n\
                 \  secure               = False\n\
                 \  requestHeaders       = []\n\
                 \  path                 = \"/users\"\n\
                 \  queryString          = \"\"\n\
                 \  method               = \"GET\"\n\
                 \  proxy                = Nothing\n\
                 \  rawBody              = False\n\
                 \  redirectCount        = 10\n\
                 \  responseTimeout      = ResponseTimeoutDefault\n\
                 \  requestVersion       = HTTP/1.1\n\
                 \  proxySecureMode      = ProxySecureWithConnect\n\
                 \}\n"
  -- foldMap @[]
  --     (<> "\r\n")
  --     [ "GET /v1/users HTTP/1.1"
  --     , "Host: api.example.com"
  --     , "Accept: application/json"
  --     , "User-Agent: haskell-openapi3-th"
  --     , ""
  --     ]
  it @Expectation "" do
    assertHttpClientWarpExchange @GetUsers () server

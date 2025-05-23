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
import Prelude (fromIntegral)

import OpenApiTH

declare $
  specFile "examples/OpenApiGuide/BasicStructure/openapi.yaml"
    <> operation ("get /users" & setOperationName "GetUsers")

server ∷ OperationServer GetUsers IO
server () = pure ["AJ", "Pat"]

spec ∷ Spec
spec = do
  it @Expectation "" do
    operationRequestBs @GetUsers [serverAddressQQ|http://api.example.com/v1|] ()
      `shouldBe` foldMap @[] (<> "\r\n")
        [ "GET /v1/users HTTP/1.1"
        , "Host: api.example.com"
        , "Accept: application/json"
        , "User-Agent: haskell-openapi3-th"
        , ""
        ]
  it @Expectation "" do
    assertHttpClientExchange @GetUsers () server

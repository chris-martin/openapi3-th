module OpenApiGuide.BasicStructure.Main where

import Essentials

import Data.ByteString.Builder qualified as BSB
import Data.Foldable
import Data.List (map)
import Data.Text (Text)
import Language.Haskell.TH qualified as TH
import List.Transformer qualified as ListT
import Network.HTTP.Simple
import Network.Wai.Handler.Warp
import OpenApiTH
import System.IO (IO)
import Test.Hspec
import Prelude (fromIntegral, show)

oath do
  declare ["GetUsers"]
  withSpecFile "examples/OpenApiGuide/BasicStructure/openapi.yaml" do
    at ["paths", "/users", "/get"] $ dub "GetUsers"

server ∷ OperationServer GetUsers IO
server () = pure ["AJ", "Pat"]

spec ∷ Spec
spec = describe "" do
  it @Expectation "round trip" do
    assertHttpClientWarpExchange @GetUsers () server

  it @Expectation "request" do
    request ← buildOperationRequest @GetUsers ()
    request.head
      `shouldBe` OutgoingRequest
        { location =
            ResourceLocation
              { scheme = Nothing
              , context = AbsoluteContext
              , path = ["users"]
              }
        , method = "GET"
        , query = []
        , accept = "application/json"
        }
    requestBody ← ListT.fold (<>) mempty id request.body
    BSB.toLazyByteString requestBody `shouldBe` ""

  it @Expectation "response" do
    response ← buildOperationResponse @GetUsers ["AJ", "Pat"]
    response.head
      `shouldBe` OutgoingResponse
        { statusCode = "200"
        , contentType = "application/json"
        }
    responseBody ← ListT.fold (<>) mempty id response.body
    BSB.toLazyByteString responseBody `shouldBe` "[\"AJ\",\"Pat\"]"

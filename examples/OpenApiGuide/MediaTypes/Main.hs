module OpenApiGuide.MediaTypes.Main where

import Essentials

import Test.Hspec

import Data.ByteString.Builder qualified as BSB
import Data.Foldable
import Data.List (map)
import Data.Text (Text)
import Language.Haskell.TH qualified as TH
import List.Transformer qualified as ListT
import Network.HTTP.Simple
import Network.Wai.Handler.Warp
import System.IO (IO)
import Prelude (fromIntegral, show)

import OpenApiTH

oath do
  declare "GetEmployees"
  withSpecFile "examples/OpenApiGuide/MediaTypes/openapi1.yaml" do
    atJsonPath ["paths", "/employees", "get"] do
      dub "GetEmployees"
      atJsonPath ["responses", "200", "content", "application/json", "schema"] do
        atJsonPath "items" $ dub "Employee"

server ∷ OperationServer GetEmployees IO
server () =
  pure
    [ Employee {id = 5, name = "Klaus", fullTime = True}
    , Employee {id = 12, name = "Niko", fullTime = False}
    ]

spec ∷ Spec
spec = describe "" do
  it @Expectation "round trip" do
    assertHttpClientWarpExchange @GetEmployees () server

  it @Expectation "request" do
    request ← buildOperationRequest @GetEmployees ()
    request.head
      `shouldBe` OutgoingRequest
        { location =
            ResourceLocation
              { scheme = Nothing
              , context = AbsoluteContext
              , path = ["employees"]
              }
        , method = "GET"
        , query = []
        , accept = "application/json"
        }
    requestBody ← ListT.fold (<>) mempty id request.body
    BSB.toLazyByteString requestBody `shouldBe` ""

  it @Expectation "response" do
    response ←
      buildOperationResponse @GetEmployees
        [ Employee {id = 5, name = "Klaus", fullTime = True}
        , Employee {id = 12, name = "Niko", fullTime = False}
        ]
    response.head
      `shouldBe` OutgoingResponse
        { statusCode = "200"
        , contentType = "application/json"
        }
    responseBody ← ListT.fold (<>) mempty id response.body
    BSB.toLazyByteString responseBody `shouldBe` "..."

module OpenApiGuide.BasicStructure.Main where

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

declare $
  specFile "examples/OpenApiGuide/BasicStructure/openapi.yaml"
    <> operation ("get /users" & setOperationName "GetUsers")

server ∷ OperationServer GetUsers IO
server () = pure ["AJ", "Pat"]

spec ∷ Spec
spec = describe "" do
  it @Expectation "" do
    assertHttpClientWarpExchange @GetUsers () server
  it @Expectation "" do
    request ← buildOperationRequest @GetUsers ()
    request.head
      `shouldBe` OutgoingRequest
        { server = Nothing
        , method = "GET"
        , path = Path [PathSegment "users"]
        , query = []
        }
    requestBody ← ListT.fold (<>) mempty id request.body
    BSB.toLazyByteString requestBody `shouldBe` ""

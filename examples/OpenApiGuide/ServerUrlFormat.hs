-- https://swagger.io/docs/specification/v3_0/api-host-and-base-path/
module OpenApiGuide.ServerUrlFormat where

import Essentials

import Test.Hspec

import Data.ByteString.Builder qualified as BSB
import Data.Either (Either (..))
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

spec ∷ Spec
spec = do
  it "" $ readServerUrl "https://api.example.com" `shouldBe` Right _
  it "" $ readServerUrl "https://api.example.com:8443/v1/reports" `shouldBe` Right _
  it "" $ readServerUrl "http://localhost:3025/v1" `shouldBe` Right _
  it "" $ readServerUrl "http://10.0.81.36/v1" `shouldBe` Right _
  it "" $ readServerUrl "ws://api.example.com/v1" `shouldBe` Right _
  it "" $ readServerUrl "wss://api.example.com/v1" `shouldBe` Right _
  it "" $ readServerUrl "/v1/reports" `shouldBe` Right _
  it "" $ readServerUrl "/" `shouldBe` Right _
  it "" $ readServerUrl "//api.example.com" `shouldBe` Right _

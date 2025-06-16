-- https://swagger.io/docs/specification/v3_0/api-host-and-base-path/
module OpenApiGuide.ServerUrlFormat where

import Essentials

import Test.Hspec

import Data.ByteString.Builder qualified as BSB
import Data.Either (Either (..))
import Data.Foldable
import Data.List (map)
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Language.Haskell.TH qualified as TH
import List.Transformer qualified as ListT
import Network.HTTP.Simple
import Network.Wai.Handler.Warp
import System.IO (IO)
import Prelude (fromIntegral, show)

import OpenApiTH

-- https://swagger.io/docs/specification/v3_0/api-host-and-base-path/
spec ∷ Spec
spec = do
  it "" $
    readResourceLocation "https://api.example.com"
      `shouldBe` Right
        ResourceLocation
          { scheme = Just "https"
          , context =
              AuthorityContext
                Authority
                  { userInfo = Nothing
                  , host = "api.example.com"
                  , port = Nothing
                  }
          , path = Empty
          }
  it "" $ readResourceLocation "https://api.example.com:8443/v1/reports" `shouldBe` Right _
  it "" $ readResourceLocation "http://localhost:3025/v1" `shouldBe` Right _
  it "" $ readResourceLocation "http://10.0.81.36/v1" `shouldBe` Right _
  it "" $ readResourceLocation "ws://api.example.com/v1" `shouldBe` Right _
  it "" $ readResourceLocation "wss://api.example.com/v1" `shouldBe` Right _
  it "" $ readResourceLocation "/v1/reports" `shouldBe` Right _
  it "" $ readResourceLocation "/" `shouldBe` Right _
  it "" $ readResourceLocation "//api.example.com" `shouldBe` Right _

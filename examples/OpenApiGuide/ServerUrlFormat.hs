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
import Optics
import System.IO (IO)
import Prelude (fromIntegral, show)

import OpenApiTH

-- https://swagger.io/docs/specification/v3_0/api-host-and-base-path/
spec ∷ Spec
spec = do
  it "" $
    readResourceLocation "https://api.example.com"
      `shouldBe` Right
        ( mempty
            & #scheme ?~ "https"
            & #context
              .~ AuthorityContext (hostAuthority "api.example.com")
        )
  it "" $
    readResourceLocation "https://api.example.com:8443/v1/reports"
      `shouldBe` Right
        ( mempty
            & #scheme ?~ "https"
            & #context
              .~ AuthorityContext
                (hostAuthority "api.example.com" & #port ?~ 8443)
            & #path .~ ["v1", "reports"]
        )
  it "" $
    readResourceLocation "http://localhost:3025/v1"
      `shouldBe` Right
        ( mempty
            & #scheme ?~ "http"
            & #context
              .~ AuthorityContext
                (hostAuthority "localhost" & #port ?~ 3025)
            & #path .~ ["v1"]
        )
  it "" $
    readResourceLocation "http://10.0.81.36/v1"
      `shouldBe` Right
        ( mempty
            & #scheme ?~ "http"
            & #context .~ AuthorityContext (hostAuthority "10.0.81.36")
            & #path .~ ["v1"]
        )
  it "" $
    readResourceLocation "ws://api.example.com/v1"
      `shouldBe` Right
        ( mempty
            & #scheme ?~ "ws"
            & #context .~ AuthorityContext (hostAuthority "api.example.com")
            & #path .~ ["v1"]
        )
  it "" $
    readResourceLocation "wss://api.example.com/v1"
      `shouldBe` Right
        ( mempty
            & #scheme ?~ "wss"
            & #context .~ AuthorityContext (hostAuthority "api.example.com")
            & #path .~ ["v1"]
        )
  it "" $
    readResourceLocation "/v1/reports"
      `shouldBe` Right
        ( mempty
            & #context .~ AbsoluteContext
            & #path .~ ["v1", "reports"]
        )
  it "" $
    readResourceLocation "/"
      `shouldBe` Right
        (mempty & #context .~ AbsoluteContext)
  it "" $
    readResourceLocation "//api.example.com"
      `shouldBe` Right
        ( mempty
            & #context .~ AuthorityContext (hostAuthority "api.example.com")
        )

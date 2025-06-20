-- https://swagger.io/docs/specification/v3_0/api-host-and-base-path/
module OpenApiGuide.ServerUrlFormat where

import Essentials

import Test.Hspec

import Data.ByteString.Builder qualified as BSB
import Data.Either (Either (..))
import Data.Foldable
import Data.List (map)
import Data.Monoid
import Data.Sequence (Seq (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Haskell.TH qualified as TH
import List.Transformer qualified as ListT
import Network.HTTP.Simple
import Network.Wai.Handler.Warp
import Optics
import System.IO (IO)
import Prelude (fromIntegral, show)

import OpenApiTH

check ∷ Text → ResourceLocation → Spec
check t x = it (Text.unpack t) $ readResourceLocation t `shouldBe` Right x

spec ∷ Spec
spec = describe "readResourceLocation" do
  check "https://api.example.com" $
    mempty
      & #scheme ?~ "https"
      & #context .~ AuthorityContext (hostAuthority "api.example.com")

  check "https://api.example.com:8443/v1/reports" $
    mempty
      & #scheme ?~ "https"
      & #context .~ AuthorityContext (hostAuthority "api.example.com" & #port ?~ 8443)
      & #path .~ ["v1", "reports"]

  check "http://localhost:3025/v1" $
    mempty
      & #scheme ?~ "http"
      & #context .~ AuthorityContext (hostAuthority "localhost" & #port ?~ 3025)
      & #path .~ ["v1"]

  check "http://10.0.81.36/v1" $
    mempty
      & #scheme ?~ "http"
      & #context .~ AuthorityContext (hostAuthority "10.0.81.36")
      & #path .~ ["v1"]

  check "ws://api.example.com/v1" $
    mempty
      & #scheme ?~ "ws"
      & #context .~ AuthorityContext (hostAuthority "api.example.com")
      & #path .~ ["v1"]

  check "wss://api.example.com/v1" $
    mempty
      & #scheme ?~ "wss"
      & #context .~ AuthorityContext (hostAuthority "api.example.com")
      & #path .~ ["v1"]

  check "/v1/reports" $
    mempty & #context .~ AbsoluteContext & #path .~ ["v1", "reports"]

  check "/" $ mempty & #context .~ AbsoluteContext

  check "//api.example.com" $
    mempty & #context .~ AuthorityContext (hostAuthority "api.example.com")

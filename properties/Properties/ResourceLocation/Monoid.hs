module Properties.ResourceLocation.Monoid where

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
import Test.QuickCheck
import Prelude (fromIntegral, show)

import OpenApiTH

spec ∷ Spec
spec = describe "ResourceLocation monoid" do
  it "associativity" do
    property \(a ∷ ResourceLocation, b, c) → (a <> b) <> c == a <> (b <> c)
  it "left identity" do
    property \(a ∷ ResourceLocation) → mempty <> a == a
  it "right identity" do
    property \(a ∷ ResourceLocation) → a <> mempty == a

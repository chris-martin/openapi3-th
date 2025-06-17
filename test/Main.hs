module Main (main) where

import Prelude

import Test.Hspec

import OpenApiGuide.BasicStructure.Main qualified
import OpenApiGuide.ServerUrlFormat qualified
import Properties.ResourceLocation.Monoid qualified

main ∷ IO ()
main = hspec do
  OpenApiGuide.BasicStructure.Main.spec
  OpenApiGuide.ServerUrlFormat.spec
  Properties.ResourceLocation.Monoid.spec

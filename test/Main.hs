module Main (main) where

import OpenApiGuide.BasicStructure.Main qualified
import OpenApiGuide.ServerUrlFormat qualified
import Properties.ResourceLocation.Monoid qualified
import Test.Hspec
import Prelude

main ∷ IO ()
main = hspec do
  OpenApiGuide.BasicStructure.Main.spec
  OpenApiGuide.ServerUrlFormat.spec
  Properties.ResourceLocation.Monoid.spec

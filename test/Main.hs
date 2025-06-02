module Main (main) where

import Prelude

import Test.Hspec

import OpenApiGuide.BasicStructure.Main qualified
import OpenApiGuide.ServerUrlFormat qualified

main ∷ IO ()
main = hspec do
  OpenApiGuide.BasicStructure.Main.spec
  OpenApiGuide.ServerUrlFormat.spec

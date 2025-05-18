module Main (main) where

import Prelude

import Test.Hspec

import OpenApiGuide.BasicStructure.Main

main ∷ IO ()
main = hspec do
  OpenApiGuide.BasicStructure.Main.spec

module OpenApiGuide.BasicStructure.Main where

import Prelude

import Test.Hspec

import Data.Text (Text)
import Language.Haskell.TH qualified as TH

import OpenApiTH qualified as OATH

do
  spec <- OATH.readSpecFile "examples/OpenApiGuide/BasicStructure/openapi.yaml"
  OATH.makeSpecPathSchemaType spec
    (SpecPath ["paths", "/users", "get", "responses", "200", "application/json", "schema"])
    schemaTypeOptions{ name = "UserNames" }

spec :: Spec
spec = it @(IO ()) "" do
  let _ = UserNames $ V.fromList ["AJ", "Pat"]
  pure ()

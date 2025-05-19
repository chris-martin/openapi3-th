module OpenApiGuide.BasicStructure.Main where

import Essentials

import Test.Hspec

import Data.Text (Text)
import Language.Haskell.TH qualified as TH
import System.IO (IO)

import OpenApiTH qualified as OATH

OATH.declare $
  OATH.specFile "examples/OpenApiGuide/BasicStructure/openapi.yaml"
    <> OATH.operation ("get /users" & OATH.setOperationName "GetUsers")

{- Generates:

data GetUsers

type instance Request GetUsers = ()

type instance Response GetUsers = Vector Text

-}

server ∷ OATH.Server GetUsers IO
server () = pure ["AJ", "Pat"]

spec ∷ Spec
spec = it @(IO ()) "" do
  makeRequestBs @GetUsers "http://api.example.com/v1" ()
    `shouldBe` fold
      [ "GET /v1/users\r\n"
      , "Host: api.example.com\r\n"
      , "Accept: application/json\r\n"
      , "\r\n"
      ]

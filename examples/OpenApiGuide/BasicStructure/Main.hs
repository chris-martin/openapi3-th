module OpenApiGuide.BasicStructure.Main where

import Essentials

import Test.Hspec

import Data.Foldable
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

server ∷ OATH.OperationServer GetUsers IO
server () = pure ["AJ", "Pat"]

request :: OATH.OperationRequest GetUsers
request = ()

spec ∷ Spec
spec = it @(IO ()) "" do
  operationRequestBs @GetUsers "http://api.example.com/v1" request
    `shouldBe` fold
      [ "GET /v1/users HTTP/1.1\r\n"
      , "Host: api.example.com\r\n"
      , "Accept: application/json\r\n"
      , "User-Agent: haskell-openapi3-th\r\n"
      , "\r\n"
      ]
  withApplication (operationWaiApplication @GetUsers) \port ->
    _

module Oath.Operation.Operation where

import Essentials

import Data.ByteString.Builder qualified as BSB
import List.Transformer
import System.IO (IO)

import Oath.Operation.IncomingRequest
import Oath.Operation.IncomingResponse
import Oath.Operation.Message
import Oath.Operation.OutgoingRequest
import Oath.Operation.OutgoingResponse

class Operation op where
  type OperationRequest op ∷ Type
  type OperationResponse op ∷ Type

  buildOperationRequest
    ∷ OperationRequest op
    → IO (Message OutgoingRequest (ListT IO BSB.Builder))

  buildOperationResponse
    ∷ OperationResponse op
    → IO (Message OutgoingResponse (ListT IO BSB.Builder))

  readOperationRequest
    ∷ Message IncomingRequest (ListT IO BSB.Builder)
    → IO (OperationRequest op)

  readOperationResponse
    ∷ Message IncomingResponse (ListT IO BSB.Builder)
    → IO (OperationResponse op)

type OperationServer op m =
  OperationRequest op → m (OperationResponse op)

module OpenApiTH.Operation.Wai where

import Essentials

import Data.ByteString (ByteString)
import List.Transformer
import Network.Wai
import System.IO (IO)

import OpenApiTH.Operation.IncomingRequest
import OpenApiTH.Operation.IncomingResponse
import OpenApiTH.Operation.Message
import OpenApiTH.Operation.Operation
import OpenApiTH.Operation.OutgoingRequest
import OpenApiTH.Operation.OutgoingResponse

waiToOperationRequest
  ∷ ∀ op
   . Operation op
  ⇒ Request
  → IO (OperationRequest op)
waiToOperationRequest = readWaiRequest >=> readOperationRequest @op

operationResponseToWai
  ∷ ∀ op
   . Operation op
  ⇒ OperationResponse op
  → IO Response
operationResponseToWai = buildOperationResponse @op >=> buildWaiResponse

buildWaiResponse ∷ Message OutgoingResponse (ListT IO ByteString) → IO Response
buildWaiResponse _ = _

readWaiRequest ∷ Request → IO (Message IncomingRequest (ListT IO ByteString))
readWaiRequest _ = _

operationWaiApplication
  ∷ ∀ op
   . Operation op
  ⇒ OperationServer op IO
  → Application
operationWaiApplication server waiRequest respond = do
  request ← waiToOperationRequest @op waiRequest
  response ← server request
  waiResponse ← operationResponseToWai @op response
  respond waiResponse

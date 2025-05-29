module OpenApiTH.Operation.Wai where

import Essentials

import Data.ByteString (ByteString)
import List.Transformer
import Network.Wai
import System.IO (IO)

import OpenApiTH.Operation.Message
import OpenApiTH.Operation.Operation

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

buildWaiResponse ∷ Message ResponseBuilder (ListT IO ByteString) → IO Response
buildWaiResponse _ = _

readWaiRequest ∷ Request → IO (Message RequestReader (ListT IO ByteString))
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

module OpenApiTH.Wai where

import Essentials

import Network.Wai
import System.IO (IO)

import OpenApiTH.Operation

class WaiOperation op where
  waiToOperationRequest ∷ Request → IO (OperationRequest op)
  operationResponseToWai ∷ OperationResponse op → IO Response

operationWaiApplication
  ∷ ∀ op
   . WaiOperation op
  ⇒ OperationServer op IO
  → Application
operationWaiApplication server waiRequest respond = do
  request ← waiToOperationRequest @op waiRequest
  response ← server request
  waiResponse ← operationResponseToWai @op response
  respond waiResponse

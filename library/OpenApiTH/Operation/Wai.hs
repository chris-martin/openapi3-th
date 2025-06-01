module OpenApiTH.Operation.Wai where

import Essentials

import Control.Monad.Fail (fail)
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.Int (Int)
import List.Transformer
import Network.HTTP.Types (Status (..))
import Network.Wai qualified as Wai
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
  ⇒ Wai.Request
  → IO (OperationRequest op)
waiToOperationRequest = readWaiRequest >=> readOperationRequest @op

operationResponseToWai
  ∷ ∀ op
   . Operation op
  ⇒ OperationResponse op
  → IO Wai.Response
operationResponseToWai = buildOperationResponse @op >=> buildWaiResponse

buildWaiResponse ∷ Message OutgoingResponse (ListT IO BSB.Builder) → IO Wai.Response
buildWaiResponse message = do
  let Message {head, body} = message
      OutgoingResponse {statusCode, contentType} = head
  statusCodeInt ← case BS.readInt statusCode of
    Just (i, r) | BS.null r → pure i
    _ → fail "Invalid status code"
  let status = Status statusCodeInt ""
  pure $ Wai.responseStream status [] (listToWaiStreamingBody message.body)

listToWaiStreamingBody ∷ ListT IO BSB.Builder → Wai.StreamingBody
listToWaiStreamingBody xs write _flush = runListT $ xs >>= lift . write

readWaiRequest ∷ Wai.Request → IO (Message IncomingRequest (ListT IO BSB.Builder))
readWaiRequest x =
  pure
    Message
      { head =
          IncomingRequest
            { server = _
            , method = _
            , path = _
            , query = _
            }
      , body = _
      }

operationWaiApplication
  ∷ ∀ op
   . Operation op
  ⇒ OperationServer op IO
  → Wai.Application
operationWaiApplication server waiRequest respond = do
  request ← waiToOperationRequest @op waiRequest
  response ← server request
  waiResponse ← operationResponseToWai @op response
  respond waiResponse

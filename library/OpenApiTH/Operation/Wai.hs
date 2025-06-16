module OpenApiTH.Operation.Wai where

import Essentials

import Control.Monad.Fail (fail)
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.Int (Int)
import Data.List qualified as List
import Data.Text (Text)
import List.Transformer
import Network.HTTP.Types (Status (..))
import Network.HTTP.Types.Header qualified as HTTP
import Network.Wai qualified as Wai
import System.IO (IO)

import OpenApiTH.OpenApi
import OpenApiTH.Operation.IncomingRequest
import OpenApiTH.Operation.IncomingResponse
import OpenApiTH.Operation.Message
import OpenApiTH.Operation.Operation
import OpenApiTH.Operation.OutgoingRequest
import OpenApiTH.Operation.OutgoingResponse
import OpenApiTH.Web.BasicAuthentication (readBasicAuthentication)
import OpenApiTH.Web.PercentEncoding (percentDecodeUtf8)

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
readWaiRequest x = do
  let host = percentDecodeUtf8 =<< Wai.requestHeaderHost x
      authorization = List.lookup HTTP.hAuthorization (Wai.requestHeaders x)
      basicAuthentication = readBasicAuthentication =<< authorization
      method = _
      path = _
      query = _
      head = IncomingRequest {host, basicAuthentication, method, path, query}
      body = _
  pure Message {head, body}

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

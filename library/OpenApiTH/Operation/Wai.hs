module OpenApiTH.Operation.Wai where

import Essentials

import Data.ByteString (ByteString)
import List.Transformer
import Network.Wai qualified as Wai
import System.IO (IO)

import OpenApiTH.Operation.IncomingRequest
import OpenApiTH.Operation.IncomingResponse
import OpenApiTH.Operation.Message
import OpenApiTH.Operation.Operation
import OpenApiTH.Operation.OutgoingRequest
import OpenApiTH.Operation.OutgoingResponse
import Network.HTTP.Types (Status(..))
import Data.Int (Int)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Fail (fail)

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

buildWaiResponse ∷ Message OutgoingResponse (ListT IO ByteString) → IO Wai.Response
buildWaiResponse message = do
  let
    Message{head,body} = message
    OutgoingResponse{statusCode,contentType}  = head
  statusCodeInt <- case BS.readInt statusCode of
    Just (i,r) | BS.null r -> pure i
    _ -> fail "Invalid status code"
  let status = Status statusCodeInt ""
  pure $ Wai.responseStream status [] \write flush -> _

readWaiRequest ∷ Wai.Request → IO (Message IncomingRequest (ListT IO ByteString))
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

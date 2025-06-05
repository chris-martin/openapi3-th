module OpenApiTH.Operation.Operation where

import Essentials

import Conduit
import Control.Monad.Fail
import Control.Monad.Yield
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BS (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (LazyByteString)
import Data.Word (Word16)
import Iri.Data (Authority, Fragment, Host, Iri, Path (..), Port, Query, Scheme, Security (..))
import List.Transformer
import System.IO (IO)

import OpenApiTH.OpenApi.ResourceLocation
import OpenApiTH.Operation.IncomingRequest
import OpenApiTH.Operation.IncomingResponse
import OpenApiTH.Operation.Message
import OpenApiTH.Operation.OutgoingRequest
import OpenApiTH.Operation.OutgoingResponse

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

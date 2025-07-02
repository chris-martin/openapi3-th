module Oath.Operation.Operation where

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
import List.Transformer
import System.IO (IO)

import Oath.Operation.IncomingRequest
import Oath.Operation.IncomingResponse
import Oath.Operation.Message
import Oath.Operation.OutgoingRequest
import Oath.Operation.OutgoingResponse
import Oath.Web

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

module OpenApiTH.Operation.Operation where

import Essentials

import Conduit
import Control.Monad.Fail
import Control.Monad.Yield
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BS (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (LazyByteString)
import Data.Word (Word16)
import Iri.Data (Authority, Fragment, Host, Iri, Path (..), Port, Query, Scheme, Security (..))
import List.Transformer
import System.IO (IO)

import OpenApiTH.OpenApi.Server
import OpenApiTH.Operation.Message
import OpenApiTH.Operation.RequestBuilder

class Operation op where
  type OperationRequest op ∷ Type
  type OperationResponse op ∷ Type
  buildOperationRequest
    ∷ OperationRequest op → IO (Message RequestBuilder (ListT IO ByteString))
  buildOperationResponse
    ∷ OperationResponse op → IO (Message ResponseBuilder (ListT IO ByteString))
  readOperationRequest
    ∷ Message RequestReader (ListT IO ByteString) → IO (OperationRequest op)
  readOperationResponse
    ∷ Message ResponseReader (ListT IO ByteString) → IO (OperationResponse op)

type OperationServer op m = OperationRequest op → m (OperationResponse op)

data ResponseBuilder = ResponseBuilder

data RequestReader

data ResponseReader

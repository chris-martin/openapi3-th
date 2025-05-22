module OpenApiTH.Request where

import Essentials

import Control.Monad.Yield
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (LazyByteString)
import Network.HTTP.Simple

import OpenApiTH.Operation
import OpenApiTH.RequestMessage
import OpenApiTH.ServerAddress

operationRequestBs ∷ ∀ op. ServerAddress → OperationRequest op → LazyByteString
operationRequestBs r s = _

operationRequestHttpClient ∷ ∀ op. ServerAddress → OperationRequest op → Request
operationRequestHttpClient = _

-- | Working with HTTP message encoding
module OpenApiTH.MessageBytes where

import Essentials

import Control.Monad.Fail
import Control.Monad.Yield
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (LazyByteString)
import Network.HTTP.Simple

import OpenApiTH.Operation
import OpenApiTH.ServerAddress

operationRequestBs ∷ ∀ op. ServerAddress → OperationRequest op → LazyByteString
operationRequestBs r s = _

bsOperationResponse
  ∷ ∀ op m
   . MonadFail m
  ⇒ LazyByteString
  → m (OperationResponse op)
bsOperationResponse bs = _

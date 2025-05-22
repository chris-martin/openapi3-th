module OpenApiTH.Response where

import Essentials

import Control.Monad.Fail
import Control.Monad.Yield
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (LazyByteString)
import Network.HTTP.Simple

import OpenApiTH.Operation
import OpenApiTH.RequestMessage
import OpenApiTH.ServerAddress

bsOperationResponse
  ∷ ∀ op m
   . MonadFail m
  ⇒ LazyByteString
  → m (OperationResponse op)
bsOperationResponse bs = _

httpClientOperationResponse
  ∷ ∀ op m
   . MonadFail m
  ⇒ Response LazyByteString
  → m (OperationResponse op)
httpClientOperationResponse = _

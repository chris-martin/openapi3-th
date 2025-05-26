-- | Working with HTTP message encoding
module OpenApiTH.Operation.MessageBytes where

import Essentials

import Control.Monad.Fail
import Control.Monad.Yield
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (LazyByteString)
import Iri.Data (Authority, Fragment, Iri, Path, Query, Scheme)

import OpenApiTH.OpenApi.ServerUrl
import OpenApiTH.Operation.Operation

data RequestBuilder = RequestBuilder
  { method ∷ ByteString
  , path ∷ Path
  , query ∷ Query
  , fragment ∷ Fragment
  , userAgent ∷ ByteString
  , body ∷ ByteString
  }

data ResponseBuilder = ResponseBuilder

operationRequestBs ∷ ∀ op. ServerUrl → OperationRequest op → LazyByteString
operationRequestBs r s = _

bsOperationResponse
  ∷ ∀ op m
   . MonadFail m
  ⇒ LazyByteString
  → m (OperationResponse op)
bsOperationResponse bs = _

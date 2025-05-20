module OpenApiTH.HttpRequest where

import Essentials

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (LazyByteString)
import Control.Monad.Yield

import OpenApiTH.Operation
import OpenApiTH.RequestMessage
import OpenApiTH.ServerAddress

httpRequest ∷ ∀ op. _ => Request op → ServerAddress → LazyByteString
httpRequest r s = _


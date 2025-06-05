module OpenApiTH.Operation.OutgoingRequest where

import Essentials

import Conduit
import Control.Monad.Fail
import Control.Monad.Yield
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.Word (Word16)
import Iri.Data (Authority, Fragment, Host, Iri, Path (..), Port, Query, Scheme, Security (..))
import Optics
import System.IO (IO)

import OpenApiTH.OpenApi.ResourceLocation

-- | Intermediate representation of a request header to be turned into
--   an HTTP message
data OutgoingRequest = OutgoingRequest
  { location ∷ ResourceLocation
  , method ∷ ByteString
  , query ∷ [(ByteString, Maybe ByteString)]
  , accept ∷ ByteString
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''OutgoingRequest

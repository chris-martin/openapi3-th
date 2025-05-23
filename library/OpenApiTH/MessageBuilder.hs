module OpenApiTH.MessageBuilder where

import Essentials

import Data.ByteString (ByteString)
import Iri.Data (Authority, Fragment, Iri, Path, Query, Scheme)

data RequestBuilder = RequestBuilder
  { method ∷ ByteString
  , path ∷ Path
  , query ∷ Query
  , fragment ∷ Fragment
  , userAgent ∷ ByteString
  , body ∷ ByteString
  }

data ResponseBuilder = ResponseBuilder

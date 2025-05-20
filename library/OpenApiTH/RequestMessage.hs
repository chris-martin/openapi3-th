module OpenApiTH.RequestMessage where

import Essentials

import Data.ByteString (ByteString)
import Iri.Data (Authority, Fragment, Iri, Path, Query, Scheme)

data RequestMessage = RequestMessge
  { method ∷ ByteString
  , path ∷ Path
  , query ∷ Query
  , fragment ∷ Fragment
  , userAgent ∷ ByteString
  , body ∷ ByteString
  }

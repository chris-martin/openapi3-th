{-# OPTIONS_GHC -Wno-missing-fields #-}

module OpenApiTH.OpenApi.Server where

import Essentials

import Control.Monad (unless)
import Control.Monad.Fail
import Control.Monad.Validate
import Data.ByteString (ByteString)
import Data.Either (Either (..), either)
import Data.String (IsString)
import Data.Text qualified as Text
import Data.Word
import Iri.Data
import Iri.Parsing.Text qualified as P
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

data Server = Server
  { security ∷ Security
  , host ∷ Host
  , port ∷ Port
  }
  deriving stock (Eq, Show)

data ServerUrl = ServerUrl
  { security ∷ Security
  , host ∷ Host
  , port ∷ Port
  , path ∷ Path
  }
  deriving stock (Lift, Eq, Show)

serverUrlQQ ∷ QuasiQuoter
serverUrlQQ =
  QuasiQuoter
    { quoteExp = \s → case P.httpIri (Text.pack s) of
        Left e → fail $ Text.unpack e
        Right (HttpIri security host port path query fragment) → do
          result ← runValidateT do
            unless (query == Query "") $ dispute ["Query must be empty"]
            unless (fragment == Fragment "") $ dispute ["Fragment must be empty"]
            pure ServerUrl {security, host, port, path}
          either (fail . Text.unpack . Text.intercalate "\n") lift result
    }

localhost ∷ ServerUrl
localhost =
  ServerUrl
    { security = Security False
    , host = NamedHost $ RegName [DomainLabel "localhost"]
    , port = MissingPort
    , path = Path []
    }

setServerPort ∷ Word16 → ServerUrl → ServerUrl
setServerPort port x = x {port = PresentPort port}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module OpenApiTH.OpenApi.ServerUrl where

import Essentials

import Control.Monad (unless)
import Control.Monad.Fail
import Control.Monad.Validate
import Data.ByteString (ByteString)
import Data.Either (Either (..), either)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word
import Iri.Data
import Iri.Parsing.Text qualified as P
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

data ServerUrl = ServerUrl
  { security ∷ Security
  , host ∷ Host
  , port ∷ Port
  , path ∷ Path
  }
  deriving stock (Lift, Eq, Show)

readServerUrl ∷ Text → Either [Text] ServerUrl
readServerUrl t = case P.httpIri t of
  Left e → Left [e]
  Right (HttpIri security host port path query fragment) → runValidate do
    unless (query == Query "") $ dispute ["Query must be empty"]
    unless (fragment == Fragment "") $ dispute ["Fragment must be empty"]
    pure ServerUrl {security, host, port, path}

serverUrlQQ ∷ QuasiQuoter
serverUrlQQ =
  QuasiQuoter
    { quoteExp =
        either (fail . Text.unpack . Text.intercalate "\n") lift
          . readServerUrl
          . Text.pack
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

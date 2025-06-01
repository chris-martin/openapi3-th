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

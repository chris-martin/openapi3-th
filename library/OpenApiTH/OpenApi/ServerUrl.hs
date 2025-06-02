{-# OPTIONS_GHC -Wno-missing-fields #-}

-- | Server URL as defined in
-- https://swagger.io/docs/specification/v3_0/api-host-and-base-path/
module OpenApiTH.OpenApi.ServerUrl where

import Essentials

import Control.Monad (unless)
import Control.Monad.Fail
import Control.Monad.Validate
import Data.ByteString (ByteString)
import Data.Either (Either (..), either)
import Data.List qualified as List
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word
import Iri.Data
import Iri.Parsing.Text qualified as P
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Megaparsec qualified as MP
import Text.URI (URI)
import Text.URI qualified as URI

data ServerUrl
  = ServerUrlWithAuthority Scheme Authority Path
  | ServerUrlAbsolute Path
  | ServerUrlRelative Path
  deriving stock (Eq, Show, Lift)

readServerUrl ∷ Text → Either [Text] ServerUrl
readServerUrl t = runValidate do
  let uriE = MP.parse (URI.parser <* MP.eof) "<URI>" t
  let invalidURI (_ ∷ MP.ParseErrorBundle Text ()) = refute ["Invalid URI (RFC 3986)"]
  uri ← either invalidURI pure uriE
  serverUrl ← do
    let schemeMaybe = (Scheme . Text.encodeUtf8 . URI.unRText) <$> uri.uriScheme
    _
  unless (List.null uri.uriQuery) $ dispute ["Query must be empty"]
  unless (List.null uri.uriFragment) $ dispute ["Fragment must be empty"]
  pure serverUrl

serverUrlQQ ∷ QuasiQuoter
serverUrlQQ =
  QuasiQuoter
    { quoteExp =
        either (fail . Text.unpack . Text.intercalate "\n") lift
          . readServerUrl
          . Text.pack
    }

localhost ∷ Port → ServerUrl
localhost port =
  ServerUrlWithAuthority
    (Scheme "http")
    ( Authority
        MissingUserInfo
        (NamedHost $ RegName [DomainLabel "localhost"])
        port
    )
    (Path [])

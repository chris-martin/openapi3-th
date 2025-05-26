-- | Integration with the @http-client@ library
module OpenApiTH.Operation.HttpClient where

import Essentials

import Conduit
import Control.Monad.Fail
import Control.Monad.Validate (refute, runValidateT)
import Control.Monad.Yield
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString qualified as BSL
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (LazyByteString)
import Data.Either (either)
import Data.Foldable (concat, fold)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as V
import Iri.Data (DomainLabel (..), Host (..), Path (..), PathSegment (..), Port (..), RegName (..), Security (..))
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Simple
import Network.URI qualified as URI
import Network.Wai.Handler.Warp
import OpenApiTH.OpenApi.Server
import OpenApiTH.Operation.Operation
import OpenApiTH.Operation.Wai
import System.IO (IO)
import Test.Hspec
import Text.Show (show)
import Prelude (fromIntegral)

buildHttpClientRequest ∷ RequestBuilder → IO HttpClient.Request
buildHttpClientRequest rb = do
  result ← runValidateT do
    server ← maybe (refute ["No server"]) pure rb.server
    pure $
      HttpClient.defaultRequest
        & setRequestMethod rb.method
        & setRequestSecure (let Security x = server.security in x)
        & setRequestHost
          ( BSL.toStrict $
              BSB.toLazyByteString $
                case server.host of
                  NamedHost x → renderRegName x
          )
        & setRequestPort
          ( case server.port of
              PresentPort x → fromIntegral x
              MissingPort → case server.security of
                Security False → 80
                Security True → 443
          )
        & setRequestPath
          ( BSL.toStrict $
              BSB.toLazyByteString $
                renderPath rb.path
          )

  either (fail . show @[Text]) pure result

readHttpClientResponse ∷ HttpClient.Response (ConduitT () ByteString IO ()) → IO ResponseReader
readHttpClientResponse rr = _

renderPath ∷ Path → BSB.Builder
renderPath =
  fold
    . fmap ((\(PathSegment x) → "/" <> BSB.byteString x))
    . V.toList
    . (\(Path xs) → xs)

renderRegName ∷ RegName → BSB.Builder
renderRegName =
  fold
    . List.intersperse "."
    . fmap
      ( BSB.byteString
          . Text.encodeUtf8
          . Text.pack
          . URI.escapeURIString URI.isUnescapedInURIComponent
          . Text.unpack
          . (\(DomainLabel x) → x)
      )
    . V.toList
    . (\(RegName xs) → xs)

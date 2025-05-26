-- | Integration with the @http-client@ library
module OpenApiTH.Operation.HttpClient where

import Essentials

import Conduit
import Control.Monad.Fail
import Control.Monad.Yield
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString qualified as BSL
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (LazyByteString)
import Data.Foldable (concat, fold)
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as V
import Iri.Data (DomainLabel (..), Host (..), Path (..), PathSegment (..), Port (..), RegName (..), Security (..))
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Simple
import Network.URI qualified as URI
import Network.Wai.Handler.Warp
import OpenApiTH.OpenApi.ServerUrl
import OpenApiTH.Operation.Operation
import OpenApiTH.Operation.Wai
import System.IO (IO)
import Test.Hspec
import Prelude (fromIntegral)

class HttpClientOperation op where
  operationRequestToHttpClient ∷ OperationRequest op → IO Request
  httpClientToOperationResponse
    ∷ Response (ConduitT () ByteString IO ()) → IO (OperationResponse op)

setHttpClientRequestServerUrl ∷ ∀ op. ServerUrl → Request → Request
setHttpClientRequestServerUrl ServerUrl {security, host, port, path} =
  setSecurity . setHost . setPort . setPath
 where
  setSecurity = setRequestSecure (let Security x = security in x)
  setHost r =
    r
      { HttpClient.host =
          BSL.toStrict $
            BSB.toLazyByteString $
              case host of
                NamedHost x → renderRegName x
      }
  setPort r =
    r
      { HttpClient.port = case port of
          PresentPort x → fromIntegral x
          MissingPort → case security of
            Security False → 80
            Security True → 443
      }
  setPath r =
    r
      { HttpClient.path =
          BSL.toStrict $
            BSB.toLazyByteString $
              renderPath path <> BSB.byteString (HttpClient.path r)
      }

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

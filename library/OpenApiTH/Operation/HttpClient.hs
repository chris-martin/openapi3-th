-- | Integration with the @http-client@ library
module OpenApiTH.Operation.HttpClient where

import Essentials

import Conduit
import Control.Applicative (empty)
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
import Data.Conduit.Internal (ConduitT (..), Pipe (..))
import Data.Either (either)
import Data.Foldable (concat, fold)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as V
import Iri.Data (DomainLabel (..), Host (..), Path (..), PathSegment (..), Port (..), RegName (..), Security (..))
import List.Transformer (ListT (..), Step (..))
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Simple
import Network.HTTP.Types.Header qualified as Http
import Network.HTTP.Types.Status qualified as Http
import Network.Wai.Handler.Warp
import System.IO (IO)
import Test.Hspec
import Text.Show (show)
import Prelude (fromIntegral)

import OpenApiTH.OpenApi.ResourceLocation
import OpenApiTH.Operation.IncomingRequest
import OpenApiTH.Operation.IncomingResponse
import OpenApiTH.Operation.Message
import OpenApiTH.Operation.Operation
import OpenApiTH.Operation.OutgoingRequest
import OpenApiTH.Operation.OutgoingResponse
import OpenApiTH.Operation.Wai

buildHttpClientRequest
  ∷ Message OutgoingRequest (ListT IO BSB.Builder) → IO HttpClient.Request
buildHttpClientRequest message = do
  result ← runValidateT do
    server ← maybe (refute ["No server"]) pure message.head.server
    pure $
      HttpClient.defaultRequest
        & setRequestMethod message.head.method
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
                renderPath message.head.path
          )
        & ( \x →
              x
                { HttpClient.requestHeaders =
                    (Http.hAccept, message.head.accept) : HttpClient.requestHeaders x
                }
          )

  either (fail . show @[Text]) pure result

readHttpClientResponse
  ∷ HttpClient.Response (ConduitT () ByteString IO ())
  → IO (Message IncomingResponse (ListT IO BSB.Builder))
readHttpClientResponse x = do
  pure
    Message
      { head =
          IncomingResponse
            { statusCode = statusBs $ HttpClient.responseStatus x
            , contentType = List.lookup Http.hContentType $ HttpClient.responseHeaders x
            }
      , body =
          let go = \case
                HaveOutput next o → ListT $ pure $ Cons (BSB.byteString o) $ go next
                NeedInput f _ → go $ f ()
                Done () → empty
                PipeM m → lift m >>= go
                Leftover _ _ → empty
           in go $ ($ Done) $ unConduitT $ HttpClient.responseBody x
      }

statusBs ∷ Http.Status → BSL.StrictByteString
statusBs = BSL.toStrict . BSB.toLazyByteString . BSB.intDec . Http.statusCode

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

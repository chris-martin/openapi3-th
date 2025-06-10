-- | Integration with the @http-client@ library
module OpenApiTH.Operation.HttpClient where

import Essentials

import Conduit
import Control.Applicative (empty)
import Control.Monad.Fail
import Control.Monad.Validate (refute, runValidateT)
import Control.Monad.Yield
import Data.ByteString (ByteString, StrictByteString)
import Data.ByteString qualified as BS
import Data.ByteString qualified as BSL
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy (LazyByteString)
import Data.Conduit.Internal (ConduitT (..), Pipe (..))
import Data.Either (either)
import Data.Foldable (concat, fold, toList)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as V
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

import Data.Bits (toIntegralSized)
import Data.Sequence (Seq)
import OpenApiTH.OpenApi
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
    scheme ← maybe (refute ["No scheme"]) pure message.head.location.scheme
    authority ← case message.head.location.context of
      AuthorityContext x → pure x
      _ → refute ["No authority"]
    secure ←
      maybe (refute ["Scheme is not HTTP"]) pure $
        List.lookup (Text.toLower scheme) [("http", False), ("https", True)]
    port ← case authority.port of
      Just x → case toIntegralSized x of
        Just y → pure y
        _ → refute ["Port out of range"]
      Nothing → pure case secure of
        False → 80
        True → 443
    pure $
      HttpClient.defaultRequest
        & setRequestMethod message.head.method
        & setRequestSecure secure
        & setRequestHost (Text.encodeUtf8 authority.host)
        & setRequestPort port
        & setRequestPath (renderPath message.head.location.path)
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

statusBs ∷ Http.Status → StrictByteString
statusBs = BSL.toStrict . BSB.toLazyByteString . BSB.intDec . Http.statusCode

renderPath ∷ Seq Text → StrictByteString
renderPath =
  BSL.toStrict
    . BSB.toLazyByteString
    . fold
    . fmap ((\x → "/" <> Text.encodeUtf8Builder x))
    . toList

-- | Integration with the @http-client@ library
module Oath.Operation.HttpClient where

import Essentials

import Conduit
import Control.Applicative (empty)
import Control.Monad.Fail
import Control.Monad.Validate (refute, runValidateT)
import Data.ByteString (ByteString, StrictByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.Conduit.Internal (ConduitT (..), Pipe (..))
import Data.Either (either)
import Data.List qualified as List
import Data.Text (Text)
import List.Transformer (ListT (..), Step (..))
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Simple
import Network.HTTP.Types.Header qualified as Http
import Network.HTTP.Types.Status qualified as Http
import Optics
import System.IO (IO)
import Text.Show (show)

import Oath.ByteString
import Oath.Http
import Oath.Operation.IncomingResponse
import Oath.Operation.Message
import Oath.Operation.OutgoingRequest
import Oath.Uri

buildHttpClientRequest
  ∷ Message OutgoingRequest (ListT IO BSB.Builder) → IO HttpClient.Request
buildHttpClientRequest message = do
  result ← runValidateT do
    let scheme = message.head.location.scheme
    authority ← case message.head.location ^? #authority of
      Just x → pure x
      _ → refute ["No authority"]
    secure ←
      maybe (refute ["Scheme is not http(s)"]) pure $
        List.lookup scheme [("http", False), ("https", True)]
    port ← case authority.port of
      Just x → case BS.readInt x of
        Just (y, r) | BS.null r → pure y
        _ → refute ["Port invalid"]
      Nothing → pure case secure of
        False → 80
        True → 443
    pure $
      HttpClient.defaultRequest
        & setRequestMethod message.head.method
        & setRequestSecure secure
        & setRequestHost (buildStrict $ renderHost authority.host)
        & setRequestPort port
        & setRequestPath (buildStrict $ renderPath $ message.head.location ^. #path)
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
statusBs = buildStrict . BSB.intDec . Http.statusCode

module OpenApiTH.Declare.Declare where

import Essentials

import Conduit qualified
import Control.Monad.Fail
import Control.Monad.Trans.Class
import Control.Monad.Yield
import Data.Aeson qualified as JSON
import Data.ByteString (ByteString)
import Data.Foldable
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tuple
import Data.Vector (Vector)
import Iri.Data (Path (..), PathSegment (..))
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax qualified as TH
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Simple qualified as HttpClient
import Network.HTTP.Types.Header qualified as Http
import Network.HTTP.Types.Status qualified as Http
import Network.Wai qualified as Wai

import OpenApiTH.Declare.Options
import OpenApiTH.ListT
import OpenApiTH.Operation.HttpClient
import OpenApiTH.Operation.IncomingRequest (IncomingRequest (IncomingRequest))
import OpenApiTH.Operation.IncomingRequest qualified as IReq
import OpenApiTH.Operation.IncomingResponse (IncomingResponse (IncomingResponse))
import OpenApiTH.Operation.IncomingResponse qualified as IResp
import OpenApiTH.Operation.Message
import OpenApiTH.Operation.Operation
import OpenApiTH.Operation.OutgoingRequest (OutgoingRequest (OutgoingRequest))
import OpenApiTH.Operation.OutgoingRequest qualified as OReq
import OpenApiTH.Operation.OutgoingResponse (OutgoingResponse (OutgoingResponse))
import OpenApiTH.Operation.OutgoingResponse qualified as OResp
import OpenApiTH.Operation.Wai

declare ∷ (ToOptions opt, MonadFail m, Quote m) ⇒ opt → m [Dec]
declare opt =
  fmap fst $ runYieldT listAggregation do
    for_ operations \op → do
      name ← lift $ maybe (fail "todo") (pure . TH.mkName . Text.unpack) op.name
      yieldM $ dataD (cxt []) name [] Nothing [] []
      yieldManyM
        [d|
          instance Operation $(conT name) where
            type OperationRequest $(conT name) = ()
            buildOperationRequest () =
              pure
                Message
                  { head =
                      OutgoingRequest
                        { OReq.server = Nothing
                        , OReq.method = "GET"
                        , OReq.path = Path [PathSegment "users"]
                        , OReq.query = []
                        }
                  , body = mempty
                  }
            readOperationRequest _ = pure ()

            type OperationResponse $(conT name) = Vector Text

            buildOperationResponse xs =
              pure
                Message
                  { head =
                      OutgoingResponse
                        { OResp.statusCode = "200"
                        , OResp.contentType = "application/json"
                        }
                  , body = lbsChunkList $ JSON.encode xs
                  }

            readOperationResponse x =
              (\case Just a → a) $
                List.lookup
                  ((x.head ∷ IncomingResponse).statusCode ∷ ByteString)
                  [ (,) "200" $ do
                      bodyLbs ← foldBsList x.body
                      case JSON.decode bodyLbs of
                        Nothing → _
                        Just r → pure r
                  ]
                  -- buildOperationResponse xs =
                  --   pure $
                  --     Wai.responseLBS
                  --       Http.ok200
                  --       [(Http.hContentType, "application/json")]
                  --       (JSON.encode xs)
                  -- readOperationResponse rr = do
                  --   let headers = HttpClient.responseHeaders httpClientResponse
                  --       contentTypeMaybe = List.lookup Http.hContentType headers
                  --       statusCode = Http.statusCode $ HttpClient.responseStatus httpClientResponse
                  --   case statusCode of
                  --     200 → case contentTypeMaybe of
                  --       Just "application/json" → do
                  --         body ←
                  --           Conduit.runConduit $
                  --             HttpClient.responseBody httpClientResponse Conduit..| Conduit.sinkLazy
                  --         case JSON.decode body of
                  --           Nothing → _
                  --           Just response → pure response
                  --       _ → _
                  --     _ → _
          |]
 where
  Options {specFile, operations} = toOptions opt

yieldM ∷ Monad m ⇒ m a → YieldT a m ()
yieldM x = yield =<< lift x

yieldManyM ∷ (Monad m, Foldable t) ⇒ m (t a) → YieldT a m ()
yieldManyM x = traverse_ yield =<< lift x

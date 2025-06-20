module OpenApiTH.Declare.Declare where

import Essentials

import Conduit qualified
import Control.Applicative (empty)
import Control.Monad.Fail
import Control.Monad.Trans.Class
import Control.Monad.Yield
import Data.Aeson qualified as JSON
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as BSB
import Data.Foldable
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tuple
import Data.Vector (Vector)
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax qualified as TH
import List.Transformer qualified as ListT
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Simple qualified as HttpClient
import Network.HTTP.Types.Header qualified as Http
import Network.HTTP.Types.Status qualified as Http
import Network.Wai qualified as Wai

import OpenApiTH.Declare.Options
import OpenApiTH.OpenApi
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
import OpenApiTH.Web

declare ∷ (MonadFail m, Quote m) ⇒ Options → m [Dec]
declare Options {specFile, annotations, declarations} =
  fmap fst $ runYieldT listAggregation do
    for_ declarations \nameText → do
      name ← lift $ pure $ TH.mkName $ Text.unpack nameText
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
                        { OReq.location =
                            ResourceLocation
                              { scheme = Nothing
                              , context = AbsoluteContext
                              , path = ["users"]
                              }
                        , OReq.method = "GET"
                        , OReq.query = []
                        , OReq.accept = "application/json"
                        }
                  , body = empty
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
                  , body = pure $ BSB.lazyByteString $ JSON.encode xs
                  }

            readOperationResponse x =
              (\case Just a → a) $
                List.lookup
                  ((x.head ∷ IncomingResponse).statusCode ∷ ByteString)
                  [ (,) "200" $ do
                      bodyBuilder ← ListT.fold (<>) mempty id x.body
                      case JSON.decode (BSB.toLazyByteString bodyBuilder) of
                        Nothing → _
                        Just r → pure r
                  ]
          |]

yieldM ∷ Monad m ⇒ m a → YieldT a m ()
yieldM x = yield =<< lift x

yieldManyM ∷ (Monad m, Foldable t) ⇒ m (t a) → YieldT a m ()
yieldManyM x = traverse_ yield =<< lift x

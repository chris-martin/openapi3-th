module Oath.Declare.Declare where

import Essentials

import Conduit qualified
import Control.Applicative (empty)
import Control.Monad.Fail
import Control.Monad.State.Strict
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

import Oath.Declare.Options
import Oath.OpenApi
import Oath.Operation.HttpClient
import Oath.Operation.IncomingRequest (IncomingRequest (IncomingRequest))
import Oath.Operation.IncomingRequest qualified as IReq
import Oath.Operation.IncomingResponse (IncomingResponse (IncomingResponse))
import Oath.Operation.IncomingResponse qualified as IResp
import Oath.Operation.Message
import Oath.Operation.Operation
import Oath.Operation.OutgoingRequest (OutgoingRequest (OutgoingRequest))
import Oath.Operation.OutgoingRequest qualified as OReq
import Oath.Operation.OutgoingResponse (OutgoingResponse (OutgoingResponse))
import Oath.Operation.OutgoingResponse qualified as OResp
import Oath.Operation.Wai
import Oath.Web

oath ∷ (MonadFail m, Quote m) ⇒ OptionsM () → m [Dec]
oath optionsM = do
  let Options {specFile, declarations, dubs} = execOptionsM optionsM
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

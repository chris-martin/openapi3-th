module OpenApiTH.Declare where

import Essentials

import Conduit qualified
import Control.Monad.Fail
import Control.Monad.Trans.Class
import Control.Monad.Yield
import Data.Aeson qualified as JSON
import Data.Foldable
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tuple
import Data.Vector (Vector)
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax qualified as TH
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Simple qualified as HttpClient
import Network.HTTP.Types.Header qualified as Http
import Network.HTTP.Types.Status qualified as Http
import Network.Wai qualified as Wai

import OpenApiTH.HttpClient
import OpenApiTH.Operation
import OpenApiTH.Options
import OpenApiTH.Wai

declare ∷ (ToOptions opt, MonadFail m, Quote m) ⇒ opt → m [Dec]
declare opt =
  fmap fst $ runYieldT listAggregation do
    for_ operations \op → do
      name ← lift $ maybe (fail "todo") (pure . TH.mkName . Text.unpack) op.name
      yieldM $ dataD (cxt []) name [] Nothing [] []
      yieldManyM
        [d|
          type instance OperationRequest $(conT name) = ()

          type instance OperationResponse $(conT name) = Vector Text

          instance WaiOperation $(conT name) where
            waiToOperationRequest _ = pure ()
            operationResponseToWai xs =
              pure $
                Wai.responseLBS
                  Http.ok200
                  [(Http.hContentType, "application/json")]
                  (JSON.encode xs)

          instance HttpClientOperation $(conT name) where
            operationRequestToHttpClient () = pure HttpClient.defaultRequest
            httpClientToOperationResponse httpClientResponse = do
              let headers = HttpClient.responseHeaders httpClientResponse
                  contentTypeMaybe = List.lookup Http.hContentType headers
                  statusCode = Http.statusCode $ HttpClient.responseStatus httpClientResponse
              case statusCode of
                200 → case contentTypeMaybe of
                  Just "application/json" → do
                    body ←
                      Conduit.runConduit $
                        HttpClient.responseBody httpClientResponse Conduit..| Conduit.sinkLazy
                    case JSON.decode body of
                      Nothing → _
                      Just response → pure response
                  _ → _
                _ → _
          |]
 where
  Options {specFile, operations} = toOptions opt

yieldM ∷ Monad m ⇒ m a → YieldT a m ()
yieldM x = yield =<< lift x

yieldManyM ∷ (Monad m, Foldable t) ⇒ m (t a) → YieldT a m ()
yieldManyM x = traverse_ yield =<< lift x

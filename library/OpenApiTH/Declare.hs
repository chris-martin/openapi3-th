module OpenApiTH.Declare where

import Essentials

import Control.Monad.Fail
import Control.Monad.Trans.Class
import Control.Monad.Yield
import Data.Aeson qualified as JSON
import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tuple
import Data.Vector (Vector)
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax qualified as TH
import Network.HTTP.Simple qualified as HttpClient
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
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
                  ok200
                  [(hContentType, "application/json")]
                  (JSON.encode xs)

          instance HttpClientOperation $(conT name) where
            operationRequestToHttpClient () = pure HttpClient.defaultRequest
            httpClientToOperationResponse x = _
          |]
 where
  Options {specFile, operations} = toOptions opt

yieldM ∷ Monad m ⇒ m a → YieldT a m ()
yieldM x = yield =<< lift x

yieldManyM ∷ (Monad m, Foldable t) ⇒ m (t a) → YieldT a m ()
yieldManyM x = traverse_ yield =<< lift x

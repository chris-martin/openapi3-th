module OpenApiTH.Declare where

import Essentials

import Control.Monad.Fail
import Control.Monad.Trans.Class
import Control.Monad.Yield
import Data.Foldable
import Data.Text qualified as Text
import Data.Tuple
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax qualified as TH

import OpenApiTH.Operation
import OpenApiTH.Options

declare ∷ (ToOptions opt, MonadFail m, Quote m) ⇒ opt → m [Dec]
declare opt =
  fmap fst $ runYieldT listAggregation do
    for_ operations \op → do
      name ← lift $ maybe (fail "todo") (pure . TH.mkName . Text.unpack) op.name
      yieldM $ dataD (cxt []) name [] Nothing [] []
      yieldM $
        tySynInstD $
          tySynEqn
            Nothing
            (appT (conT ''Request) (conT name))
            (conT ''())
 where
  Options {specFile, operations} = toOptions opt

yieldM ∷ Monad m ⇒ m a → YieldT a m ()
yieldM x = yield =<< lift x

module OpenApiTH.Declare where

import Essentials

import Language.Haskell.TH

import Control.Monad.Fail
import OpenApiTH.Options

declare ∷ ToOptions opt ⇒ (MonadFail m, Quote m) ⇒ opt → m [Dec]
declare opt =
  [d|data A|]
 where
  Options {specFile, operations} = toOptions opt

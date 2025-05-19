module OpenApiTH.Spec where

import Essentials

import Data.Aeson qualified as JSON

data Spec = Spec {value ∷ JSON.Value}

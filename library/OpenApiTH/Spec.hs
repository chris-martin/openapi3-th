module OpenApiTH.Spec where

import Prelude

import Data.Aeson qualified as JSON

data Spec = Spec{ value :: JSON.Value }

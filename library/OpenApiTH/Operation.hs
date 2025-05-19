module OpenApiTH.Operation where

import Essentials

type family Request op ∷ Type

type family Response op ∷ Type

type Server op m = Request op → m (Response op)

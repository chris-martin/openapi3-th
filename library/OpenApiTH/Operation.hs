module OpenApiTH.Operation where

import Essentials

type family OperationRequest op ∷ Type

type family OperationResponse op ∷ Type

type OperationServer op m = OperationRequest op → m (OperationResponse op)

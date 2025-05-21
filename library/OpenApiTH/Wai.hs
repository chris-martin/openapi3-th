module OpenApiTH.Wai where

import Essentials

import Network.Wai
import System.IO (IO)

import OpenApiTH.Operation

operationWaiApplication ∷ ∀ op. OperationServer op IO → Application
operationWaiApplication = _

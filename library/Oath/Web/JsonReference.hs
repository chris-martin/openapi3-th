-- | A JSON reference is a URI reference whose fragment is a JSON pointer.
module Oath.Web.JsonReference where

import Essentials

import Data.Either (Either)
import Data.Text (Text)

import Oath.Web.JsonPointer

data JsonReference
  deriving stock (Eq, Ord)

readJsonReference ∷ Text → Either [Text] JsonReference
readJsonReference = _

module OpenApiTH.Operation.Message where

import Essentials

import Optics.TH

data Message head body = Message {head ∷ head, body ∷ body}
  deriving stock (Eq, Show)

makeLensesFor [("head", "messageHead"), ("body", "messageBody")] ''Message

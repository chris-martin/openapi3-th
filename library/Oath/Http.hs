-- | <https://www.rfc-editor.org/rfc/rfc7230.html>
-- Hypertext Transfer Protocol (HTTP/1.1): Message Syntax and Routing
module Oath.Http where

import Essentials

import Data.ByteString (ByteString)
import Data.Sequence

import Oath.Grammar
import Oath.Uri.Path

-- | Render a path for an HTTP request in origin form
-- <https://www.rfc-editor.org/rfc/rfc7230.html#section-5.3.1>
renderPath ∷ Seq ByteString → Builder ByteString
renderPath =
  forceRenderCanonical pathAbemptyGrammar
    . \case Empty → singleton ""; xs → xs

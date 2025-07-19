module Oath.Uri.Rfc3986.Uri where

import Essentials

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Either (Either (..))
import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text (Text)

import Oath.Uri.Rfc3986.Grammar (AbsoluteUri (..), Authority (..))
import Oath.Uri.Rfc3986.Grammar qualified as G
import Oath.Uri.Rfc3986.Path

data Uri = Uri
  { scheme ∷ ByteString
  , authority ∷ Maybe Authority
  , path ∷ Path
  , query ∷ Maybe ByteString
  , fragment ∷ Maybe ByteString
  }

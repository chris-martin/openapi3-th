{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module OpenApiTH.SpecPath where

import Prelude

import Data.Aeson (Key)
import Data.Aeson.Key qualified as Key
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural (Natural)

data SpecPath = SpecPath {items ∷ [SpecPathItem]}

data SpecPathItem
  = SpecPathKey Key
  | SpecPathIndex Natural

instance IsString SpecPathItem where
  fromString = SpecPathKey . fromString

instance Num SpecPathItem where
  fromInteger = SpecPathIndex . fromInteger

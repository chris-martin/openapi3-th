{-# options_ghc -fno-warn-missing-methods #-}

module OpenApiTH.SpecPath where

import Prelude

import Data.String
import Data.Text qualified as Text
import Data.Text (Text)
import Numeric.Natural (Natural)
import Data.Aeson (Key)
import Data.Aeson.Key qualified as Key

data SpecPath = SpecPath{ items :: [SpecPathItem] }

data SpecPathItem =
  SpecPathKey Key
  | SpecPathIndex Natural

instance IsString SpecPathItem where
  fromString = SpecPathKey . fromString

instance Num SpecPathItem where
  fromInteger = SpecPathIndex . fromInteger

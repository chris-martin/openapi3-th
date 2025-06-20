{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module OpenApiTH.OpenApi.SpecPath where

import Essentials

import Data.Aeson (Key)
import Data.Aeson.Key qualified as Key
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.IsList
import Numeric.Natural (Natural)
import Prelude (Num (..))

data SpecPath = SpecPath {items ∷ [SpecPathItem]}
  deriving stock (Eq, Ord)

instance IsList SpecPath where
  type Item SpecPath = SpecPathItem
  fromList = SpecPath

data SpecPathItem
  = SpecPathKey Key
  | SpecPathIndex Natural
  deriving stock (Eq, Ord)

instance IsString SpecPathItem where
  fromString = SpecPathKey . fromString

instance Num SpecPathItem where
  fromInteger = SpecPathIndex . fromInteger

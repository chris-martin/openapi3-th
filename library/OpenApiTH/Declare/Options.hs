module OpenApiTH.Declare.Options where

import Essentials

import Data.Map.Strict (Map)
import Data.Sequence (Seq (..))
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import System.IO (FilePath)
import Prelude (error)

import OpenApiTH.OpenApi

data Options = Options
  { specFile ∷ FilePath
  , declarations ∷ Seq Text
  , annotations ∷ Map SpecPath Annotation
  }

data Annotation
  = Annotation {name ∷ Text}

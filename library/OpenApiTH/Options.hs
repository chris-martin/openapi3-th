module OpenApiTH.Options where

import Essentials

import Data.Sequence (Seq (..))
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import System.IO (FilePath)
import Prelude (error)

data Options = Options
  { specFile ∷ Maybe FilePath
  , operations ∷ Seq OperationOptions
  }

data OperationOptions = OperationOptions
  { method ∷ Text
  , path ∷ Text
  , name ∷ Maybe Text
  }

setOperationName ∷ Text → OperationOptions → OperationOptions
setOperationName n o = o {name = Just n}

instance IsString OperationOptions where
  fromString s = case Text.words (Text.pack s) of
    [method, path] → OperationOptions {method, path, name = Nothing}
    _ → error "No parse"

class ToOptions a where
  toOptions ∷ a → Options

instance ToOptions Options where
  toOptions = id

defaultOptions ∷ Options
defaultOptions =
  Options
    { specFile = Nothing
    , operations = Empty
    }

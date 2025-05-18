module OpenApiTH.Options where

newtype OptionsBuilder = OptionsBuilder (Options → Options)

data Options = Options
  { specFile ∷ Maybe FilePath
  , operations ∷ Seq OperationOptions
  }

data OperationOptions = OperationOptions
  { method ∷ Text
  , path ∷ Text
  , name ∷ Maybe Text
  }

class ToOptions a where
  toOptions ∷ a → Options

instance ToOptions Options where
  toOptions = id

instance ToOptions OptionsBuilder where
  toOptions (OptionsBuilder f) = f defaultOptions

defaultOptions =
  Options
    { specFile = Nothing
    , operations = Nothing
    }

module OpenApiTH.OptionsBuilder where

import Essentials

import Data.Monoid
import Data.Sequence (Seq (..))
import Data.Text (Text)
import System.IO (FilePath)

import OpenApiTH.Options (OperationOptions, Options (..), ToOptions (..), defaultOptions)
import OpenApiTH.Options qualified as Opt

newtype OptionsBuilder = OptionsBuilder (Options → Options)
  deriving (Semigroup, Monoid) via Dual (Endo Options)

instance ToOptions OptionsBuilder where
  toOptions (OptionsBuilder f) = f defaultOptions

specFile ∷ FilePath → OptionsBuilder
specFile x = OptionsBuilder \o → o {Opt.specFile = Just x}

operation ∷ OperationOptions → OptionsBuilder
operation x = OptionsBuilder \o → o {Opt.operations = o.operations :|> x}

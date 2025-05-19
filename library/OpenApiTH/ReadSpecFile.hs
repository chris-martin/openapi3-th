module OpenApiTH.ReadSpecFile where

import Essentials

import Control.Monad (when)
import Control.Monad.Fail
import Data.Aeson qualified as JSON
import Data.Either
import Data.Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String
import Data.Yaml qualified as YAML
import System.FilePath qualified as FilePath
import System.IO

import OpenApiTH.Spec

readSpecFile ∷ FilePath → IO Spec
readSpecFile file = do
  let ext = FilePath.takeExtension file
  when (null ext) $ fail "No file extension"
  case Map.lookup ext extensionFormatMap of
    Nothing → fail $ "Unrecognized file extension" <> ext
    Just fmt → case fmt of
      JSON → readSpecFileJson file
      YAML → readSpecFileYaml file

data Format = JSON | YAML

extensionFormatMap ∷ Map String Format
extensionFormatMap =
  Map.fromList
    [ (".json", JSON)
    , (".yaml", YAML)
    , (".yml", YAML)
    ]

readSpecFileJson ∷ FilePath → IO Spec
readSpecFileJson file = do
  result ← JSON.eitherDecodeFileStrict file
  case result of
    Left err → fail err
    Right value → pure Spec {value}

readSpecFileYaml ∷ FilePath → IO Spec
readSpecFileYaml file = do
  result ← YAML.decodeFileEither file
  case result of
    Left err → fail $ YAML.prettyPrintParseException err
    Right value → pure Spec {value}

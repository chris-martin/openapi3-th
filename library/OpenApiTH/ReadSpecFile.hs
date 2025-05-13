module OpenApiTH.ReadSpecFile where

import Prelude

import Data.Aeson qualified as JSON
import Data.Yaml qualified as YAML
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import System.FilePath qualified as FilePath
import Control.Monad (when)

import OpenApiTH.Spec

readSpecFile :: FilePath -> IO Spec
readSpecFile file = do
  let ext = FilePath.takeExtension file
  when (null ext) $ fail "No file extension"
  case Map.lookup ext extensionFormatMap of
    Nothing -> fail $ "Unrecognized file extension" <> ext
    Just fmt -> case fmt of
      JSON -> readSpecFileJson file
      YAML -> readSpecFileYaml file

data Format = JSON | YAML

extensionFormatMap :: Map String Format
extensionFormatMap = Map.fromList
  [ (".json", JSON)
  , (".yaml", YAML)
  , (".yml", YAML)
  ]

readSpecFileJson :: FilePath -> IO Spec
readSpecFileJson file = do
  result <- JSON.eitherDecodeFileStrict file
  case result of
    Left err -> fail err
    Right value -> pure Spec{ value }

readSpecFileYaml :: FilePath -> IO Spec
readSpecFileYaml file = do
  result <- YAML.decodeFileEither file
  case result of
    Left err -> fail $ YAML.prettyPrintParseException err
    Right value -> pure Spec{ value }

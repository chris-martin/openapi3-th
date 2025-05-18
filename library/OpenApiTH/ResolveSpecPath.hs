module OpenApiTH.ResolveSpecPath where

import Prelude

import Data.Aeson qualified as JSON
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Vector qualified as Vector

import OpenApiTH.Spec
import OpenApiTH.SpecPath

resolveSpecPath ∷ MonadFail m ⇒ Spec → SpecPath → m JSON.Value
resolveSpecPath spec specPath =
  maybe (fail "Spec path resolution failed") pure $
    resolveSpecPathMaybe spec specPath

resolveSpecPathMaybe ∷ Spec → SpecPath → Maybe JSON.Value
resolveSpecPathMaybe =
  \Spec {value} SpecPath {items} → go items value
 where
  go [] = Just
  go (x : xs) = case x of
    SpecPathKey y → \case
      JSON.Object z → KeyMap.lookup y z >>= go xs
      _ → Nothing
    SpecPathIndex y → \case
      JSON.Array z → z Vector.!? fromIntegral y >>= go xs

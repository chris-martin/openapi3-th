module Oath.Declare.Options where

import Essentials

import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import Data.Sequence (Seq (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import System.IO (FilePath)
import Prelude (error)

import Oath.OpenApi
import Oath.Relation (Relation)
import Oath.Relation qualified as Relation

data Options = Options
  { specFile ∷ Maybe FilePath
  , declarations ∷ Set Text
  , dubs ∷ Relation Text JsonReference
  }

data Annotation
  = Annotation {name ∷ Text}

newtype OptionsM a = OptionsM (State Options a)
  deriving newtype (Functor, Applicative, Monad, MonadState Options)

execOptionsM ∷ OptionsM () → Options
execOptionsM (OptionsM s) =
  execState s Options {specFile = Nothing, declarations = Set.empty, dubs = Relation.empty}

useSpecFile ∷ FilePath → OptionsM ()
useSpecFile fp = modify \o → o {specFile = Just fp}

declare ∷ Text → OptionsM ()
declare x = modify \o → o {declarations = Set.insert x o.declarations}

dub ∷ Text → JsonReference → OptionsM ()
dub a b = modify \o → o {dubs = Relation.insert a b o.dubs}

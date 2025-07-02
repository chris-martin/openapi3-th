module Oath.Relation where

import Essentials

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

data Relation a b = Relation (Map a (Set b)) (Map b (Set a))

instance (Ord a, Ord b) ⇒ Semigroup (Relation a b) where
  Relation m1 m2 <> Relation m3 m4 =
    Relation (Map.unionWith Set.union m1 m3) (Map.unionWith Set.union m2 m4)

instance (Ord a, Ord b) ⇒ Monoid (Relation a b) where
  mempty = empty

insert ∷ (Ord a, Ord b) ⇒ a → b → Relation a b → Relation a b
insert a b (Relation m1 m2) =
  Relation
    (Map.alter (Just . maybe (Set.singleton b) (Set.insert b)) a m1)
    (Map.alter (Just . maybe (Set.singleton a) (Set.insert a)) b m2)

empty ∷ Relation a b
empty = Relation Map.empty Map.empty

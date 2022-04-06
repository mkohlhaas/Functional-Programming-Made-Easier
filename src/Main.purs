module Main where

import Data.Boolean (otherwise)
import Data.Foldable (class Foldable, foldlDefault, foldrDefault, foldl, foldMap)
import Data.List (List(..), (:), singleton)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.NonEmpty ((:|))
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Ord, class Semiring, type (~>), Unit, discard, flip, negate, show, zero, ($), (+), (<>), (>))

data Tree a = Leaf a | Node (Tree a) (Tree a)
newtype RFTree a = RFTree (Tree a) -- RightFirstTree; breadth-first search
newtype LFTree a = LFTree (Tree a) -- LeftFirstTree; depth-first search

class ToList f where
  toList :: ∀ a. f a -> List a

---------------
-- Functions --
---------------

reverse :: List ~> List
reverse = foldl (flip (:)) Nil

max :: ∀ a. Ord a => a -> a -> a
max x y | x > y = x
        | otherwise = y

findMax :: ∀ a. Ord a => a -> List a -> a
findMax = foldl max

findMaxNE :: ∀ a. Ord a => NonEmptyList a -> a
findMaxNE (NonEmptyList (a :| as)) = findMax a as

sum :: ∀ a f. Foldable f => Semiring a => f a -> a
sum = foldl (+) zero

-------------------------
-- Typeclass Instances --
-------------------------

instance Foldable LFTree where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f (LFTree (Leaf x)) = f x
  foldMap f (LFTree (Node l r)) = foldMap f (LFTree l) <> foldMap f (LFTree r)

instance Foldable RFTree where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f (RFTree (Leaf x)) = f x
  foldMap f (RFTree (Node l r)) = foldMap f (RFTree r) <> foldMap f (RFTree l)

instance Foldable Tree where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l <> foldMap f r

instance ToList LFTree where
  toList = foldMap singleton

instance ToList RFTree where
  toList = foldMap singleton

instance ToList Tree where
  toList = foldMap singleton

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Exercise Chapter 11."
  log $ show $ reverse (10 : 20 : 30 : Nil)                                                    -- (30 : 20 : 10 : Nil)
  log $ show $ max (-1) 99                                                                     -- 99
  log $ show $ max "aa" "z"                                                                    -- "z"
  log $ show $ findMax 0  (37 : 311 : -1 : 2 : 84 : Nil)                                       -- 311 (0  is default value)
  log $ show $ findMax "" ("a" : "bbb" : "c" : Nil)                                            -- "c" ("" is default value)
  log $ show $ findMaxNE (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))                      -- 311
  log $ show $ findMaxNE (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))                           -- "c"
  log $ show $ sum (1   : 2   : 3   : Nil)                                                     -- 6
  log $ show $ sum (1.0 : 2.0 : 3.0 : Nil)                                                     -- 6.0
  log $ show $ toList          (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))   -- (5 : -1 : 14 : 99 : Nil)
  log $ show $ sum             (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))   -- 117
  log $ show $ toList $ LFTree (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))   -- (5 : -1 : 14 : 99 : Nil)
  log $ show $ sum    $ LFTree (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))   -- 117
  log $ show $ toList $ RFTree (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))   -- (99 : 14 : -1 : 5 : Nil)
  log $ show $ sum    $ RFTree (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))   -- 117

module Main where

import Prelude (Unit, class Semiring, class Ord)

import Data.Foldable (class Foldable)
import Data.List (List)
import Data.Semigroup.Foldable (class Foldable1)
import Effect (Effect)
import Effect.Console (log)

data Tree a = Leaf a | Node (Tree a) (Tree a)
newtype RFTree a = RFTree (Tree a) -- RightFirstTree; breadth-first search
newtype LFTree a = LFTree (Tree a) -- LeftFirstTree; depth-first search

class ToList f where
  toList :: ∀ a. f a -> List a

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Chapter 11."
  -- log $ show $ reverse (10 : 20 : 30 : Nil)                                                    -- (30 : 20 : 10 : Nil)
  -- log $ show $ max (-1) 99                                                                     -- 99
  -- log $ show $ max "aa" "z"                                                                    -- "z"
  -- log $ show $ findMax 0  (37 : 311 : -1 : 2 : 84 : Nil)                                       -- 311 (0  is default value)
  -- log $ show $ findMax "" ("a" : "bbb" : "c" : Nil)                                            -- "c" ("" is default value)
  -- log $ show $ findMaxNE (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))                      -- 311
  -- log $ show $ findMaxNE (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))                           -- "c"
  -- log $ show $ sum (1   : 2   : 3   : Nil)                                                     -- 6
  -- log $ show $ sum (1.0 : 2.0 : 3.0 : Nil)                                                     -- 6.0
  -- log $ show $ toList          (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))   -- (5 : -1 : 14 : 99 : Nil)
  -- log $ show $ sum             (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))   -- 117
  -- log $ show $ toList $ LFTree (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))   -- (5 : -1 : 14 : 99 : Nil)
  -- log $ show $ sum    $ LFTree (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))   -- 117
  -- log $ show $ toList $ RFTree (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))   -- (99 : 14 : -1 : 5 : Nil)
  -- log $ show $ sum    $ RFTree (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))   -- 117

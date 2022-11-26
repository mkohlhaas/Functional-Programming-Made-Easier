module Main where

import Data.Eq (class Eq)
import Data.Show (class Show)
import Data.Ord (class Ord)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, (==), (<=), (<), (>), (>=), ($))

-------------------------------------------------------------
-- Define data types Maybe and Either. Do not import them! --
-------------------------------------------------------------

----------------
-- Data Types --
----------------

data Maybe a = Nothing | Just a
data Either a b = Left a | Right b

---------------------
-- Class Instances --
---------------------

derive instance Eq a ⇒ Eq (Maybe a)
derive instance (Eq a, Eq b) ⇒ Eq (Either a b)
derive instance (Ord a, Ord b) ⇒ Ord (Either a b)
derive instance Ord a ⇒ Ord (Maybe a)
derive instance Generic (Maybe a) _
derive instance Generic (Either a b) _

instance Show a ⇒ Show (Maybe a) where
  show = genericShow

instance (Show a, Show b) ⇒ Show (Either a b) where
  show = genericShow

----------
-- Main --
----------

main ∷ Effect Unit
main = do
  log "Exercise Chapter 7."
  log "Derive missing instances! Only necessary imports are allowed."
  log $ show $ Just 5 == Just 5 -------------------------- true
  log $ show $ Just 5 == Just 2 -------------------------- false
  log $ show $ Just 5 == Nothing ------------------------- false
  log $ show $ Nothing == Just 5 ------------------------- false
  log $ show $ Nothing == (Nothing ∷ Maybe Unit) --------- true
  log $ show $ (Left "left" ∷ Either _ Unit) ------------- (Left "left")
  log $ show $ (Right (Just 42) ∷ Either Unit _) --------- (Right (Just 42))
  log $ show $ Left 1 < (Left 2 ∷ Either _ Unit) --------- true
  log $ show $ Left 2 < (Left 1 ∷ Either _ Unit) --------- false
  log $ show $ Just 1 < Just 5 --------------------------- true
  log $ show $ Just 5 <= Just 5 -------------------------- true
  log $ show $ Just 5 > Just 10 -------------------------- false
  log $ show $ Just 10 >= Just 10 ------------------------ true
  log $ show $ Just 99 > Nothing ------------------------- true
  log $ show $ Just 99 < Nothing ------------------------- false
  log $ show $ Just "abc" -------------------------------- (Just "abc")
  log $ show $ (Nothing ∷ Maybe Unit) -------------------- Nothing

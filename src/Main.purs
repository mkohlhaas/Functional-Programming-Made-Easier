module Main where

import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, (<>))

-------------------
-- JS Primitives --
-------------------

foreign import ordIntImpl ∷ Ordering → Ordering → Ordering → Int → Int → Ordering
foreign import eqIntImpl ∷ Int → Int → Boolean
foreign import showIntImpl ∷ Int → String
foreign import showStringImpl ∷ String → String

----------------
-- Data Types --
----------------

data Ordering = LT | EQ | GT
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b

------------------
-- Type Classes --
------------------
class Eq a where
  eq ∷ a → a → Boolean

class Eq a ⇐ Ord a where
  compare ∷ a → a → Ordering

class Show a where
  show ∷ a → String

---------------------
-- Class Instances --
---------------------

--------
-- Eq --
--------

instance Eq Int where
  eq = eqIntImpl

instance Eq Unit where
  eq _ _ = true

instance Eq Boolean where
  eq true true = true
  eq false false = true
  eq _ _ = false

instance Eq Ordering where
  eq LT LT = true
  eq EQ EQ = true
  eq GT GT = true
  eq _ _ = false

instance Eq a ⇒ Eq (Maybe a) where
  eq Nothing Nothing = true
  eq (Just x) (Just y) = x == y
  eq _ _ = false

instance (Eq a, Eq b) ⇒ Eq (Either a b) where
  eq (Left x) (Left y) = x == y
  eq (Right x) (Right y) = x == y
  eq _ _ = false

---------
-- Ord --
---------

instance Ord Int where
  compare = ordIntImpl LT EQ GT

instance Ord Unit where
  compare _ _ = EQ

instance (Ord a) ⇒ Ord (Maybe a) where
  compare Nothing Nothing = EQ
  compare (Just x) (Just y) = x `compare` y
  compare Nothing _ = LT
  compare _ Nothing = GT

instance (Ord a, Ord b) ⇒ Ord (Either a b) where
  compare (Left x) (Left y) = x `compare` y
  compare (Right x) (Right y) = x `compare` y
  compare (Left _) _ = LT
  compare _ _ = GT

----------
-- Show --
----------

instance Show Int where
  show = showIntImpl

instance Show String where
  show = showStringImpl

instance Show Unit where
  show _ = "unit"

instance Show Boolean where
  show true = "true"
  show false = "false"

instance Show a ⇒ Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "(Just " <> show a <> ")"

instance (Show a, Show b) ⇒ Show (Either a b) where
  show (Left a) = "(Left " <> show a <> ")"
  show (Right b) = "(Right " <> show b <> ")"

---------------
-- Functions --
---------------
apply ∷ ∀ a b. (a → b) → a → b
apply f a = f a

lessThan ∷ ∀ a. Ord a ⇒ a → a → Boolean
lessThan x y = x `compare` y == LT

lessThanOrEq ∷ ∀ a. Ord a ⇒ a → a → Boolean
lessThanOrEq x y = x `compare` y /= GT

greaterThan ∷ ∀ a. Ord a ⇒ a → a → Boolean
greaterThan x y = x `compare` y == GT

greaterThanOrEq ∷ ∀ a. Ord a ⇒ a → a → Boolean
greaterThanOrEq x y = x `compare` y /= LT

notEq ∷ ∀ a. Eq a ⇒ a → a → Boolean
notEq x y = (x == y) == false

---------------
-- Infix Ops --
---------------

infixr 0 apply as $
infix 4 eq as ==
infix 4 notEq as /=
infixl 4 lessThan as <
infixl 4 lessThanOrEq as <=
infixl 4 greaterThan as >
infixl 4 greaterThanOrEq as >=

----------
-- Main --
----------

main ∷ Effect Unit
main = do
  log "Exercise Chapter 7."
  log "Implement ALL missing standard 'parts' by hand! No further imports necessary!"
  log $ show $ Just 5 == Just 5 ---------------------- true
  log $ show $ Just 5 == Just 2 ---------------------- false
  log $ show $ Just 5 == Nothing --------------------- false
  log $ show $ Nothing == Just 5 --------------------- false
  log $ show $ Nothing == (Nothing ∷ Maybe Unit) ----- true
  log $ show $ (Left "left" ∷ Either _ Unit) --------- (Left "left")
  log $ show $ (Right (Just 42) ∷ Either Unit _) ----- (Right (Just 42))
  log $ show $ Left 1 < (Left 2 ∷ Either _ Unit) ----- true
  log $ show $ Left 2 < (Left 1 ∷ Either _ Unit) ----- false
  log $ show $ Just 1 < Just 5 ----------------------- true
  log $ show $ Just 5 <= Just 5 ---------------------- true
  log $ show $ Just 5 > Just 10 ---------------------- false
  log $ show $ Just 10 >= Just 10 -------------------- true
  log $ show $ Just 99 > Nothing --------------------- true
  log $ show $ Just 99 < Nothing --------------------- false
  log $ show $ Just "abc" ---------------------------- (Just "abc")
  log $ show $ (Nothing ∷ Maybe Unit) ---------------- Nothing

module Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

newtype Validation err result = Validation (Either err result)

type FamilyAgesRow r = (fatherAge ∷ Age, motherAge ∷ Age, childAge ∷ Age | r)
type FamilyNamesRow r = (fatherName ∷ FullName, motherName ∷ FullName, childName ∷ FullName | r)

newtype Age = Age Int
newtype FullName = FullName String
newtype Family = Family { | FamilyNamesRow (FamilyAgesRow ()) }
newtype FamilyAges = FamilyAges { | FamilyAgesRow () }
newtype LowerAge = LowerAge Int
newtype UpperAge = UpperAge Int
data FamilyMember = Father | Mother | Child

-- Write `validateAge` function
-- validateAge ∷ LowerAge → UpperAge → FamilyMember → Age → Validation (Array String) Age

----------
-- Main --
----------
main ∷ Effect Unit
main = do
  log "Exercise Chapter 17."
  log "No further imports allowed!!!"
  log "------------------------------------"
  log "-- Applicative Instance for Maybe --"
  log "------------------------------------"
  log $ show $ (+) <$> Just 21 <*> Just 21 -------------- (Just 42)
  log $ show $ (*) <$> pure 2 <*> pure 21 --------------- (Just 42)
  log $ show $ pure (+) <*> Just 17 <*> Just 25 --------- (Just 42)
  log "-------------------------------------"
  log "-- Applicative Instance for Either --"
  log "-------------------------------------"
  log "Associative Composition Law: (<<<) <$> u <*> v <*> w = u <*> (v <*> w)"
  log $ show $ ((<<<) <$> pure identity <*> pure identity <*> pure 1) == (pure identity <*> (pure identity <*> pure 1))
  log "Identity Law: pure identity <*> x = x"
  log $ show $ (pure identity <*> pure 1) == pure 1
  log "Homomorphism Law: pure (f x) = pure f <*> pure x"
  log $ show $ (pure (negate 1)) == (pure negate <*> pure 1)
  log "Interchange Law: u <*> pure x = pure (_ $ x) <*> u"
  log $ show $ (pure negate <*> pure 1) == (pure (_ $ 1) <*> pure negate)
  log "----------------"
  log "-- Validation --"
  log "----------------"
  log $ show $ createFamilyAges { fatherAge: Age 40, motherAge: Age 30, childAge: Age 10 } --- (Validation (Right (FamilyAges { childAge: 10, fatherAge: 40, motherAge: 30 })))
  log $ show $ createFamilyAges { fatherAge: Age 400, motherAge: Age 300, childAge: Age 0 } -- (Validation (Left ["Father is too old", "Mother is too old", "Child is too young"]))
  log $ show $ createFamilyAges { fatherAge: Age 4, motherAge: Age 3, childAge: Age 10 } ----- (Validation (Left ["Father is too young", "Mother is too young"]))
  log $ show $ createFamilyAges { fatherAge: Age 40, motherAge: Age 30, childAge: Age 100 } -- (Validation (Left ["Child is too old"]))
  log $ show $ createFamilyAges { fatherAge: Age 40, motherAge: Age 3, childAge: Age 0 } ----- (Validation (Left ["Mother is too young", "Child is too young"]))

module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

-----------------------------------------------------
-- Define Maybe and Derive All Necessary Instances --
-----------------------------------------------------

------------------------------------------------------
-- Define Either and Derive All Necessary Instances --
------------------------------------------------------

----------------
-- Validation --
----------------

-- newtype Validation err result = Validation (Either err result)

type FamilyAgesRow  r = ( fatherAge ::  Age,       motherAge  :: Age,       childAge  :: Age      | r )
type FamilyNamesRow r = ( fatherName :: FullName , motherName :: FullName , childName :: FullName | r)

newtype Age          = Age Int
newtype FullName     = FullName String
newtype Family       = Family     { | FamilyNamesRow (FamilyAgesRow ()) }
newtype FamilyAges   = FamilyAges { | FamilyAgesRow () }
newtype LowerAge     = LowerAge Int
newtype UpperAge     = UpperAge Int
data    FamilyMember = Father | Mother | Child

--------------------------------------------------------------------
-- Create and Derive Necessary Instances for the Validation Types --
--------------------------------------------------------------------

-- create also a bifunctor instance (we need it later)

-- Write validateAge function
-- validateAge :: LowerAge -> UpperAge -> Age -> FamilyMember -> Validation (Array String) Age

-- Write createFamilyAges function
-- createFamilyAges :: { | FamilyAgesRow () } -> Validation (Array String) FamilyAges

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Ch. 17."
  -- log "------------------------------"
  -- log "Applicative Instance for Maybe"
  -- log "------------------------------"
  -- log $ show $ (+) <$> Just 21 <*> Just 21                     -- (Just 42)
  -- log $ show $ (*) <$> pure 2 <*> (pure 21 :: Maybe Int)       -- (Just 42)
  -- log $ show $ pure (+) <*> Just 17 <*> Just 25                -- (Just 42)
  -- log "-------------------------------"
  -- log "Applicative Instance for Either"
  -- log "-------------------------------"
  -- -- Associative Composition Law: (<<<) <$> u <*> v <*> w = u <*> (v <*> w)
  -- log $ show $ ((<<<) <$> pure identity <*> pure identity <*> pure 1) == (pure identity <*> (pure identity <*> pure 1) :: Either Unit Int)
  -- -- Identity Law:                pure identity <*> x = x
  -- log $ show $ (pure identity <*> pure 1) == (pure 1 :: Either Unit Int)
  -- -- Homomorphism Law:            pure (f x) = pure f <*> pure x
  -- log $ show $ pure (negate 1) == (pure negate <*> pure 1 :: Either Unit Int)
  -- -- Interchange Law:             u <*> pure x = pure (_ $ x) <*> u
  -- log $ show $ (pure negate <*> pure 1) == (pure (_ $ 1) <*> pure negate :: Either Unit Int)
  -- log "----------"
  -- log "Validation"
  -- log "----------"
  -- log $ show $ createFamilyAges { fatherAge: Age 40,  motherAge: Age 30,  childAge: Age 10  }  -- (Validation (Right (FamilyAges { childAge: (Age 10), fatherAge: (Age 40), motherAge: (Age 30) })))
  -- log $ show $ createFamilyAges { fatherAge: Age 400, motherAge: Age 300, childAge: Age 0   }  -- (Validation (Left ["Father is too old", "Mother is too old", "Child is too young"]))
  -- log $ show $ createFamilyAges { fatherAge: Age 4,   motherAge: Age 3,   childAge: Age 10  }  -- (Validation (Left ["Father is too young", "Mother is too young"]))
  -- log $ show $ createFamilyAges { fatherAge: Age 40,  motherAge: Age 30,  childAge: Age 100 }  -- (Validation (Left ["Child is too old"]))
  -- log $ show $ createFamilyAges { fatherAge: Age 40,  motherAge: Age 3,   childAge: Age 0   }  -- (Validation (Left ["Mother is too young", "Child is too young"]))

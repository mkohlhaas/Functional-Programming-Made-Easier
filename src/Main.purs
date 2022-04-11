module Main where

import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

-------------------------------------------
-- Define Maybe and Either - no imports! --
-------------------------------------------

-----------
-- Maybe --
-----------
data Maybe a = Nothing | Just a

derive instance Generic (Maybe a) _

instance Show a => Show (Maybe a) where
  show = genericShow

instance Functor Maybe where
  map = liftA1

instance Apply Maybe where
  apply (Just f) (Just a) = Just $ f a
  apply _ _ = Nothing

instance Applicative Maybe where
  pure = Just

------------
-- Either --
------------
data Either a b = Left a | Right b

derive instance Generic (Either a b) _

instance (Show a, Show b) => Show (Either a b) where
  show = genericShow

derive instance (Eq a, Eq b) => Eq (Either a b)

instance Functor (Either a) where
  map f (Right b) = Right $ f b
  map _ (Left a) = Left a

instance Apply (Either a) where
  apply (Right f) (Right b) = Right $ f b
  apply (Left a) _ = Left a
  apply _ (Left a) = Left a

instance Applicative (Either a) where
  pure = Right

instance Bifunctor Either where
   bimap f _ (Left l) = Left $ f l
   bimap _ g (Right r) = Right $ g r

----------------
-- Validation --
----------------

newtype Validation err result = Validation (Either err result)

type FamilyAgesRow r = (fatherAge :: Age, motherAge :: Age, childAge :: Age | r)
type FamilyNamesRow r = (fatherName :: FullName, motherName :: FullName, childName :: FullName | r)

newtype Age = Age Int
newtype FullName = FullName String
newtype Family = Family { | FamilyNamesRow (FamilyAgesRow ()) }
newtype FamilyAges = FamilyAges { | FamilyAgesRow () }
newtype LowerAge = LowerAge Int
newtype UpperAge = UpperAge Int
data FamilyMember = Father | Mother | Child

derive newtype instance (Show a, Show b) => Show (Validation a b)
derive newtype instance Functor (Validation a)
derive newtype instance Bifunctor Validation
derive newtype instance Show Age
derive newtype instance Show FamilyAges
derive instance Generic FamilyMember _
instance Show FamilyMember where
  show = genericShow


instance Monoid a => Apply (Validation a) where
  apply (Validation (Right f)) (Validation (Right result)) = Validation $ Right (f result)
  apply (Validation (Left err1)) (Validation (Left err2)) = Validation $ Left (err1 <> err2)
  apply (Validation (Left err)) _ = Validation $ Left err
  apply _ (Validation (Left err)) = Validation $ Left err

-- Write validateAge function
validateAge :: LowerAge -> UpperAge -> Age -> FamilyMember -> Validation (Array String) Age
validateAge (LowerAge la) (UpperAge ua) age@(Age a) fm
  | a < la = Validation (Left [ show fm <> " is too young" ])
  | a > ua = Validation (Left [ show fm <> " is too old" ])
  | otherwise = Validation $ Right age

createFamilyAges :: { | FamilyAgesRow () } -> Validation (Array String) FamilyAges
createFamilyAges { fatherAge, motherAge, childAge } =
  FamilyAges <$>
    ( { fatherAge: _, motherAge: _, childAge: _ }
        <$> validateAge (LowerAge 18) (UpperAge 100) fatherAge Father
        <*> validateAge (LowerAge 18) (UpperAge 100) motherAge Mother
        <*> validateAge (LowerAge 1) (UpperAge 18) childAge Child
    )

----------
-- Main --
----------
main :: Effect Unit
main = do
  log "Exercise Chapter 17."
  log "------------------------------------"
  log "-- Applicative Instance for Maybe --"
  log "------------------------------------"
  log $ show $ (+) <$> Just 21 <*> Just 21             -- (Just 42)
  log $ show $ (*) <$> pure 2 <*> (pure 21 :: Maybe _) -- (Just 42)
  log $ show $ pure (+) <*> Just 17 <*> Just 25        -- (Just 42)
  log "-------------------------------------"
  log "-- Applicative Instance for Either --"
  log "-------------------------------------"
  log "Associative Composition Law: (<<<) <$> u <*> v <*> w = u <*> (v <*> w)"
  log $ show $ ((<<<) <$> pure identity <*> pure identity <*> pure 1) == (pure identity <*> (pure identity <*> (pure 1 :: Either Unit _)))
  log "Identity Law: pure identity <*> x = x"
  log $ show $ (pure identity <*> pure 1) == (pure 1 :: Either Unit _)
  log "Homomorphism Law: pure (f x) = pure f <*> pure x"
  log $ show $ (pure (negate 1)) == (pure negate <*> (pure 1 :: Either Unit _))
  log "Interchange Law: u <*> pure x = pure (_ $ x) <*> u"
  log $ show $ (pure negate <*> (pure 1 :: Either Unit _)) == (pure (_ $ 1) <*> pure negate)
  log "----------------"
  log "-- Validation --"
  log "----------------"
  log $ show $ createFamilyAges { fatherAge: Age 40, motherAge: Age 30, childAge: Age 10 }  -- (Validation (Right (FamilyAges { childAge: (Age 10), fatherAge: (Age 40), motherAge: (Age 30) })))
  log $ show $ createFamilyAges { fatherAge: Age 400, motherAge: Age 300, childAge: Age 0 } -- (Validation (Left ["Father is too old", "Mother is too old", "Child is too young"]))
  log $ show $ createFamilyAges { fatherAge: Age 4, motherAge: Age 3, childAge: Age 10 }    -- (Validation (Left ["Father is too young", "Mother is too young"]))
  log $ show $ createFamilyAges { fatherAge: Age 40, motherAge: Age 30, childAge: Age 100 } -- (Validation (Left ["Child is too old"]))
  log $ show $ createFamilyAges { fatherAge: Age 40, motherAge: Age 3, childAge: Age 0 }    -- (Validation (Left ["Mother is too young", "Child is too young"]))

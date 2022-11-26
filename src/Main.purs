module Main where

import Data.Eq (class Eq)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.String.Read (class Read, read)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, bind, discard, pure, show, (#), ($), (<$>), (==))

newtype CSV = CSV String
newtype FullName = FullName String
newtype Age = Age Int
data Occupation = Doctor | Dentist | Lawyer | Unemployed
data Person = Person { name ∷ FullName, age ∷ Age, occupation ∷ Occupation }

class ToCSV a where
  toCSV ∷ a → CSV

class FromCSV a where
  fromCSV ∷ CSV → Maybe a

--------
-- Eq --
--------

derive instance Eq FullName
derive instance Eq Age
derive instance Eq Occupation
derive instance Eq Person
derive instance Eq CSV

----------
-- Show --
----------

derive newtype instance Show Age
derive newtype instance Show CSV

instance Show FullName where
  show (FullName str) = str

derive instance Generic Occupation _

instance Show Occupation where
  show = genericShow

----------
-- Read --
----------

instance Read FullName where
  read str = Just $ FullName str

instance Read Age where
  read str = Age <$> fromString str

instance Read Occupation where
  read "Doctor" = Just Doctor
  read "Dentist" = Just Dentist
  read "Lawyer" = Just Lawyer
  read "Unemployed" = Just Unemployed
  read _ = Nothing

instance Read Person where
  read str = case split (Pattern ",") str of
    [ name', age', occupation' ] → do
      name ← read name'
      age ← read age'
      occupation ← read occupation'
      pure $ Person { name, age, occupation }
    _ → Nothing

---------
-- CSV --
---------

instance ToCSV Person where
  toCSV (Person { name, age, occupation }) = CSV $ intercalate "," [show name, show age, show occupation]

instance FromCSV Person where
  fromCSV (CSV p) = read p

----------
-- Main --
----------
main ∷ Effect Unit
main = do
  log "Chapter 7."
  log "Convert Person to CSV and back again. Imports are allowed and necessary."
  log $ show $ toCSV (Person { name: FullName "Sue Smith", age: Age 23, occupation: Doctor }) == CSV "Sue Smith,23,Doctor" --- true
  log $ show $ toCSV (Person { name: FullName "Sue Smith", age: Age 23, occupation: Doctor }) -------------------------------- "Sue Smith,23,Doctor"
  let person = Person { name: FullName "Sue Smith", age: Age 23, occupation: Doctor }
  log $ show $ (toCSV person # fromCSV) == Just person ----------------------------------------------------------------------- true

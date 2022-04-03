module Main where

import Data.Eq (class Eq)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, bind, discard, pure, show, (#), ($), (<$>), (==))

-----------------
-- Data Types --
-----------------

newtype CSV = CSV String
newtype FullName = FullName String
newtype Age = Age Int
data Occupation = Doctor | Dentist | Lawyer | Unemployed
data Person = Person { name :: FullName, age :: Age, occupation :: Occupation }

------------------
-- Type Classes --
------------------

class ToCSV a where
  toCSV :: a -> CSV

class FromCSV a where
  fromCSV :: CSV -> Maybe a

--------------------------
-- Type Class Instances --
--------------------------

derive instance Eq CSV
derive instance Eq FullName
derive instance Eq Age
derive instance Eq Occupation
derive instance Eq Person
derive instance Generic CSV _
derive instance Generic FullName _
derive instance Generic Age _
derive instance Generic Occupation _
derive instance Generic Person _

instance Show CSV where
  show (CSV str) = str

instance Show FullName where
  show (FullName name) = name

instance Show Age where
  show (Age age) = show age

instance Show Occupation where
  show = genericShow

instance Show Person where
  show (Person { name, age, occupation }) = intercalate "," [ show name, show age, show occupation ]

instance ToCSV Person where
  toCSV person = CSV $ show person

instance FromCSV Person where
  fromCSV (CSV str) = readPerson str

---------------
-- Functions --
---------------

readPerson :: String -> Maybe Person
readPerson str = case split (Pattern ",") str of
  [ name', age', occupation' ] -> do
    let name = FullName name'
    age <- readAge age'
    occupation <- readOccupation occupation'
    pure $ Person { name, age, occupation }
  _ -> Nothing

readAge :: String -> Maybe Age
readAge str = Age <$> fromString str

readOccupation :: String -> Maybe Occupation
readOccupation "Doctor" = Just Doctor
readOccupation "Dentist" = Just Dentist
readOccupation "Lawyer" = Just Lawyer
readOccupation "Unemployed" = Just Unemployed
readOccupation _ = Nothing

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Chapter 7."
  log "Convert Person to CSV and back again. Imports are allowed and necessary."
  log $ show $ toCSV (Person { name: FullName "Sue Smith", age: Age 23, occupation: Doctor }) == CSV "Sue Smith,23,Doctor"  -- true
  log $ show $ toCSV (Person { name: FullName "Sue Smith", age: Age 23, occupation: Doctor })                               -- "Sue Smith,23,Doctor"
  let person = Person { name: FullName "Sue Smith", age: Age 23, occupation: Doctor }
  log $ show $ (toCSV person # fromCSV) == Just person                                                                      -- true

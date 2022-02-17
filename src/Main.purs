module Main where

import Prelude (Unit, discard)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console (log)

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

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Chapter 7."
  log "Convert Person to CSV and back again."
  -- log $ show $ toCSV (Person { name: FullName "Sue Smith" , age: Age 23 , occupation: Doctor }) == CSV "Sue Smith,23,Doctor"
  -- log $ show $ toCSV (Person { name: FullName "Sue Smith" , age: Age 23 , occupation: Doctor }) -- "Sue Smith,23,Doctor"
  -- let person = Person { name: FullName "Sue Smith" , age: Age 23 , occupation: Doctor }
  -- log $ show $ (toCSV person # fromCSV) == Just person

module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((:))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Data.Traversable (class Traversable)
import Data.Unfoldable (class Unfoldable, none)
import Effect (Effect)
import Effect.Console (log)
import Parser (class ParserError, PError(..), Parser, alphaNum, count, count', digit, fail, invalidChar, parse', satisfy)

---------------------
-- Date Structures --
---------------------

-- The parsed strings are not guaranteed to be a valid dates. That's beyond the scope of this problem.
-- In the real world, we'd first parse the date and then use a date library for validation.

newtype Year = Year Int
newtype Month = Month Int
newtype Day = Day Int
data DateFormat = YearFirst | MonthFirst
type DateParts = { year ∷ Year, month ∷ Month, day ∷ Day, format ∷ DateFormat }

---------------------------
-- Show and Eq instances --
---------------------------

-- This is for testing purposes.
-- Note: Show instances for records are automagically generated.

derive instance Eq Year
derive instance Eq Month
derive instance Eq Day
derive instance Eq DateFormat
derive newtype instance Show Year
derive newtype instance Show Month
derive newtype instance Show Day
derive instance Generic DateFormat _
instance Show DateFormat where
  show = genericShow

------------------------
-- Parser Combinators --
------------------------

-- 1. Create a parser that always succeeds and returns a default in case of a failing parser.
-- optional ∷ ∀ e a. a → Parser e a → Parser e a

-- 2. Create a function that parses at most a specified count.
-- atMost ∷ ∀ e a. Int → Parser e a → Parser e (Array a)

-- 3. Specialize atMost.
-- atMost' ∷ ∀ e. Int → Parser e Char → Parser e String

-- 4. Generalize atMost on Array and pass a cons-like function to it. (There isn't one in the standard library for Unfoldable.)
-- Comment out the previous version.
-- atMost ∷ ∀ e f a. Unfoldable f ⇒ (a → f a → f a) → Int → Parser e a → Parser e (f a)

-- 5. Write a generic function that parses a min and max amount of parses with a given parser.
-- range ∷ ∀ e a. ParserError e ⇒ Int → Int → Parser e a → Parser e (Array a)

-- 6. Specialize range for String the same way you did with atMost.
-- range' ∷ ∀ e. ParserError e ⇒ Int → Int → Parser e Char → Parser e String

-- 7. Generalize range the same way you did with atMost.
-- range ∷ ∀ e f a. Semigroup (f a) ⇒ Traversable f ⇒ Unfoldable f ⇒ (a → f a → f a) → Int → Int → Parser e a → Parser e (f a)

------------------
-- Date Parsers --
------------------

-- 8. Write a Parser that parses a character but does not return anything.
-- constChar ∷ ∀ e. ParserError e ⇒ Char → Parser e Unit

-- 9. Write a parser that can parse dates in the following format: YYYY-MM-DD, M and D could be single chars.
-- 1962-10-02, 1962-10-2, 1962-9-2, ...
-- yearFirst ∷ ∀ e. ParserError e ⇒ Parser e DateParts

-- 10. Write a function that takes a String and return its integer value and refactor yearFirst.
-- `fromString` will always succeed as it has been parsed as such.
-- digitsToNum ∷ String → Int

-- 11. Same for different date format: MM/DD/YYYY.
-- 10/02/1962, 10/2/1962, 9/2/1962, ...
-- monthFirst ∷ ∀ e. ParserError e ⇒ Parser e DateParts

-- 12. Create a parser that can parse both date formats.
-- date ∷ ∀ e. ParserError e ⇒ Parser e DateParts

----------
-- Main --
----------

main ∷ Effect Unit
main = do
  log "Exercise Chapter 19 - Date Parser."
  log $ show $ parse' (optional ' ' alphaNum) "a1b2c3" == (Right (Tuple "1b2c3" 'a'))
  log $ show $ parse' (optional ' ' alphaNum) "$_$" == (Right (Tuple "$_$" ' '))
  log $ show $ parse' (atMost' (-2) alphaNum) "a1b2c3" == (Right (Tuple "a1b2c3" ""))
  log $ show $ parse' (atMost' 2 alphaNum) "$_$" == (Right (Tuple "$_$" ""))
  log $ show $ parse' (atMost' 2 alphaNum) "a1b2c3" == (Right (Tuple "b2c3" "a1"))
  log $ show $ parse' (range' 1 3 alphaNum) "a1b2c3" == (Right (Tuple "2c3" "a1b"))
  log $ show $ parse' (range' 3 3 alphaNum) "a1b2c3" == (Right (Tuple "2c3" "a1b"))
  log $ show $ parse' (range' 4 3 alphaNum) "a1b2c3" == (Right (Tuple "a1b2c3" ""))
  log $ show $ parse' yearFirst "1962-10-02" == (Right (Tuple "" { day: Day 2, format: YearFirst, month: Month 10, year: Year 1962 }))
  log $ show $ parse' yearFirst "1999-12-31" == (Right (Tuple "" { day: Day 31, format: YearFirst, month: Month 12, year: Year 1999 }))
  log $ show $ parse' monthFirst "10/2/1962" == (Right (Tuple "" { day: Day 2, format: MonthFirst, month: Month 10, year: Year 1962 }))
  log $ show $ parse' monthFirst "12/31/1999" == (Right (Tuple "" { day: Day 31, format: MonthFirst, month: Month 12, year: Year 1999 }))
  log $ show $ parse' date "1962-10-02" == (Right (Tuple "" { day: Day 2, format: YearFirst, month: Month 10, year: Year 1962 }))
  log $ show $ parse' date "10/2/1962" == (Right (Tuple "" { day: Day 2, format: MonthFirst, month: Month 10, year: Year 1962 }))
  log $ show $ parse' date "1999-12-31" == (Right (Tuple "" { day: Day 31, format: YearFirst, month: Month 12, year: Year 1999 }))
  log $ show $ parse' date "12/31/1999" == (Right (Tuple "" { day: Day 31, format: MonthFirst, month: Month 12, year: Year 1999 }))
  log $ show $ parse' date "12-31-1999" == (Left (InvalidChar "not a valid date"))

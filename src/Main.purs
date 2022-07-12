module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((:))
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray, singleton)
import Data.Traversable (class Traversable)
import Data.Unfoldable (class Unfoldable, none)
import Effect (Effect)
import Effect.Console (log)
import Parser (class ParserError, Parser, alphaNum, count, count', digit, fail, invalidChar, parse', satisfy)

-----------------
-- Date Parser --
-----------------

-- Keep in mind that these are the parts that we parsed out of a String. This is not guaranteed to be a valid
-- date. That's beyond the scope of this problem. In the real world, we'd first parse the date and then use a
-- date library to do the hard work of validation for us.

newtype Year = Year Int
newtype Month = Month Int
newtype Day = Day Int
data DateFormat = YearFirst | MonthFirst
type DateParts = { year :: Year, month :: Month, day :: Day, format :: DateFormat }

-- 1. Write Show instances for the above data types.
derive newtype instance Show Year
derive newtype instance Show Month
derive newtype instance Show Day
derive instance Generic DateFormat _

instance Show DateFormat where
  show = genericShow

----------------------
-- Helper Functions --
----------------------

-- 2. Create a parser that always succeeds and returns a default in case of a failing parser.
optional :: ∀ e a. a -> Parser e a -> Parser e a
optional def p = p <|> pure def

-- 3. Create a function that parses at most a specified count.
-- atMost :: ∀ e a. Int -> Parser e a -> Parser e (Array a)
-- atMost n p | n > 0 = optional [] $ p >>= \c -> (c : _) <$> atMost (n - 1) p
--            | otherwise = pure []

-- 4. Specialize atMost.
atMost' :: ∀ e. Int -> Parser e Char -> Parser e String
atMost' n p = fromCharArray <$> atMost (:) n p

-- 5. Generalize atMost on Array and pass a cons-like function to it (bc there isn't in the standard library for Unfoldable).
-- Comment out the previous version.
-- Unfoldable does not have a Cons operator like Array so in the first parameter we pass one.
atMost :: ∀ e f a. Unfoldable f => (a -> f a -> f a) -> Int -> Parser e a -> Parser e (f a)
atMost cons n p
  | n > 0 = optional none $ p >>= \c -> cons c <$> atMost cons (n - 1) p
  | otherwise = pure none

-- 6. Write a generic function that parses a min and max amount of parses with a given parser.
-- range :: ∀ e a. Int -> Int -> Parser e a -> Parser e (Array a)
-- range min max p
--   | min >= 0, max > 0, min <= max = append <$> count min p <*> atMost (:) (max - min) p
--   | otherwise = pure []

-- 7. Specialize range for String the same way you did with atMost.
range' :: ∀ e. Int -> Int -> Parser e Char -> Parser e String
range' min max p = fromCharArray <$> range (:) min max p

-- 8. Generalize range the same way you did with atMost.
range :: ∀ e f a. Semigroup (f a) => Traversable f => Unfoldable f => (a -> f a -> f a) -> Int -> Int -> Parser e a -> Parser e (f a)
range cons min max p
  | min >= 0, max > 0, min <= max = append <$> count min p <*> atMost (:) (max - min) p
  | otherwise = pure none

----------------------
-- Two Date Parsers --
----------------------

-- 9. Write a Parser that parses a character but does not return anything.
constChar :: ∀ e. ParserError e => Char -> Parser e Unit
constChar c = void $ satisfy (singleton c) (_ == c)

-- 10. Write a parser that can parse dates in the following format: YYYY-MM-DD, M and D could be single chars.
-- 1962-10-02, 1962-10-2, 1962-9-2, ...
yearFirst :: ∀ e. ParserError e => Parser e DateParts
yearFirst = do
  year <- Year <<< digitsToNum <$> count' 4 digit
  constChar '-'
  month <- Month <<< digitsToNum <$> range' 1 2 digit
  constChar '-'
  day <- Day <<< digitsToNum <$> range' 1 2 digit
  pure $ { year, month, day, format: YearFirst }

-- 11. Write a function that takes a String and return its integer value and refactor yearFirst.
-- The constant (-1) is pretty much irrelevant. We know that it will never be used for date values.
digitsToNum :: String -> Int
digitsToNum = fromMaybe (-1) <<< fromString

-- 12. Same for different date format: MM/DD/YYYY.
-- 10/02/1962, 10/2/1962, 9/2/1962, ...
monthFirst :: ∀ e. ParserError e => Parser e DateParts
monthFirst = do
  month <- Month <<< digitsToNum <$> range' 1 2 digit
  constChar '/'
  day <- Day <<< digitsToNum <$> range' 1 2 digit
  constChar '/'
  year <- Year <<< digitsToNum <$> count' 4 digit
  pure $ { year, month, day, format: MonthFirst }

-------------------------
-- Generic Date Parser --
-------------------------

-- 13. Create a parser that can parse both date formats.
date :: ∀ e. ParserError e => Parser e DateParts
date = yearFirst <|> monthFirst <|> fail (invalidChar "not a valid date")

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Exercise Chapter 19 - Date Parser."
  log $ show $ ({ year: Year 1921, month: Month 10, day: Day 10, format: YearFirst } :: DateParts) --- { day: 10, format: YearFirst, month: 10, year: 1921 }
  log $ show $ { year: Year 1921, month: Month 10, day: Day 10, format: YearFirst } ------------------ { day: 10, format: YearFirst, month: 10, year: 1921 }
  log $ show $ parse' (optional ' ' alphaNum) "a1b2c3" ----------------------------------------------- (Right (Tuple "1b2c3" 'a'))
  log $ show $ parse' (optional ' ' alphaNum) "$_$" -------------------------------------------------- (Right (Tuple "$_$" ' '))
  log $ show $ parse' (range' 1 3 alphaNum) "a1b2c3" ------------------------------------------------- (Right (Tuple "2c3" "a1b"))
  log $ show $ parse' (range' 3 3 alphaNum) "a1b2c3" ------------------------------------------------- (Right (Tuple "2c3" "a1b"))
  log $ show $ parse' (range' 4 3 alphaNum) "a1b2c3" ------------------------------------------------- (Right (Tuple "a1b2c3" ""))
  log $ show $ parse' (atMost' (-2) alphaNum) "a1b2c3" ----------------------------------------------- (Right (Tuple "a1b2c3" ""))
  log $ show $ parse' (atMost' 2 alphaNum) "$_$" ----------------------------------------------------- (Right (Tuple "$_$" ""))
  log $ show $ parse' (atMost' 2 alphaNum) "a1b2c3" -------------------------------------------------- (Right (Tuple "b2c3" "a1"))
  log $ show $ parse' yearFirst "1962-10-02" --------------------------------------------------------- (Right (Tuple "" { day: 2, format: YearFirst, month: 10, year: 1962 }))
  log $ show $ parse' yearFirst "1999-12-31" --------------------------------------------------------- (Right (Tuple "" { day: 31, format: YearFirst, month: 12, year: 1999 }))
  log $ show $ parse' monthFirst "10/2/1962" --------------------------------------------------------- (Right (Tuple "" { day: 2, format: MonthFirst, month: 10, year: 1962 }))
  log $ show $ parse' monthFirst "12/31/1999" -------------------------------------------------------- (Right (Tuple "" { day: 31, format: MonthFirst, month: 12, year: 1999 }))
  log $ show $ parse' date "1962-10-02" -------------------------------------------------------------- (Right (Tuple "" { day: 2, format: YearFirst, month: 10, year: 1962 }))
  log $ show $ parse' date "10/2/1962" --------------------------------------------------------------- (Right (Tuple "" { day: 2, format: MonthFirst, month: 10, year: 1962 }))
  log $ show $ parse' date "1999-12-31" -------------------------------------------------------------- (Right (Tuple "" { day: 31, format: YearFirst, month: 12, year: 1999 }))
  log $ show $ parse' date "12/31/1999" -------------------------------------------------------------- (Right (Tuple "" { day: 31, format: MonthFirst, month: 12, year: 1999 }))
  log $ show $ parse' date "12-31-1999" -------------------------------------------------------------- (Left (InvalidChar "not a valid date"))

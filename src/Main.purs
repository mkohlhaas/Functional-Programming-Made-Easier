module Main where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.CodePoint.Unicode (isDecDigit, isAlpha)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (uncons, fromCharArray)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, none, replicate)
import Effect (Effect)
import Effect.Console (log)

---------------------------------
-- Data Types and Type Classes --
---------------------------------
class ParserError (e :: Type) where
  eof :: e
  invalidChar :: String -> e

data PError
  = EOF
  | InvalidChar String

type ParserState a = Tuple String a

type ParseFunction e a = ParserError e => String -> Either e (ParserState a)

newtype Parser e a = Parser (ParseFunction e a)

data Threeple a b c = Threeple a b c

derive instance genericThreeple :: Generic (Threeple a b c) _

instance showThreeple :: (Show a, Show b, Show c) => Show (Threeple a b c) where
  show = genericShow

derive instance genericPError :: Generic PError _

instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF
  invalidChar s = InvalidChar s

instance functorParser :: Functor (Parser e) where
  map f g = Parser \s -> map f <$> parse g s

instance applyParser :: Apply (Parser e) where
  apply f g =
    Parser \s -> do
      Tuple s1 h <- parse f s
      Tuple s2 a <- parse g s1
      pure $ Tuple s2 $ h a

instance applicativeParser :: Applicative (Parser e) where
  pure a = Parser \s -> Right $ Tuple s a

instance bindParser :: Bind (Parser e) where
  bind p f =
    Parser \s -> do
      Tuple s1 x <- parse p s
      parse (f x) s1

instance monadParser :: Monad (Parser e)

instance altParser :: Alt (Parser e) where
  alt p1 p2 =
    Parser \s -> case parse p1 s of
      Left _ -> parse p2 s
      Right x -> Right x

-----------------------------------------
-- Helper Functions for Monadic Parser --
-----------------------------------------

parse :: ∀ e a. Parser e a -> ParseFunction e a
parse (Parser f) = f

parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' = parse

char :: ∀ e. Parser e Char
char =
  Parser \s -> case uncons s of
    Nothing -> Left eof
    Just { head, tail } -> Right $ Tuple tail head

count :: ∀ e a f. Traversable f => Unfoldable f => Int -> Parser e a -> Parser e (f a)
count n p
  | n < 0 = pure none
  | otherwise = sequence (replicate n p)

count' :: ∀ e. Int -> Parser e Char -> Parser e String
count' n p = fromCharArray <$> count n p

satisfy :: ∀ e. ParserError e => String -> (Char -> Boolean) -> Parser e Char
satisfy expected pred = char >>= \c -> if pred c then pure c else fail $ invalidChar expected

fail :: ∀ e a. ParserError e => e -> Parser e a
fail e = Parser $ const $ Left e

digit :: ∀ e. ParserError e => Parser e Char
digit = satisfy "digit" $ isDecDigit <<< codePointFromChar

letter :: ∀ e. ParserError e => Parser e Char
letter = satisfy "letter" $ isAlpha <<< codePointFromChar

alphaNum :: ∀ e. ParserError e => Parser e Char
alphaNum = letter <|> digit <|> fail (invalidChar "alphaNum")

-----------------
-- Date Parser --
-----------------

-- Keep in mind that these are the parts that we parsed out of a String. This is not guaranteed to be a valid
-- date. That's beyond the scope of this problem. In the real world, we'd first parse the date and then use a
-- date library to do the hard work of validation for us.

newtype Year = Year Int

newtype Month = Month Int

newtype Day = Day Int

data DateFormat
  = YearFirst
  | MonthFirst

type DateParts = { year :: Year, month :: Month, day :: Day, format :: DateFormat }

-- 1. Derive Show instances for the data types above

----------------------
-- Helper Functions --
----------------------

-- 2. Create a parser that always succeeds and returns a default in case of a failing parser
-- optional :: ∀ e a. a -> Parser e a -> Parser e a

-- 3. Create a function that parses at most a specified count
-- atMost :: ∀ e a. Int -> Parser e a -> Parser e (Array a)

-- 4. Specialize atMost; use String instead of Array
-- atMost' :: ∀ e. Int -> Parser e Char -> Parser e String

-- 5. Generalize atMost on Array an pass a cons-like function to it (bc there isn't in the standard library for Unfoldable)
-- comment out the previous version
-- atMost :: ∀ e f a. Unfoldable f => (a -> f a -> f a) -> Int -> Parser e a -> Parser e (f a)

-- 6. Write a generic function that parses a min and max amount of parses with a given parser
-- range :: ∀ e a. Int -> Int -> Parser e a -> Parser e (Array a)

-- 7. Specialize range for String the same way you did with atMost
-- range' :: ∀ e. Int -> Int -> Parser e Char -> Parser e String

-- 8. Generalize range the same way you did with atMost
-- range :: ∀ e f a. Semigroup (f a) => Traversable f => Unfoldable f => (a -> f a -> f a) -> Int -> Int -> Parser e a -> Parser e (f a)

-- 9. Write a Parser that parses a character but does not return anything
-- constChar :: ∀ e. ParserError e => Char -> Parser e Unit

-- 10. Write a function that takes a String and return its integer value
-- digitsToNum :: String -> Int

----------------------
-- Two Date Parsers --
----------------------

-- 11. Write a parser that can parse dates in the following format: YYYY-MM-DD, M and D could be single chars
-- 1962-10-02, 1962-10-2, 1962-9-2, ...
-- yearFirst :: ∀ e. ParserError e => Parser e DateParts

-- 12. Same for different date format: MM/DD/YYYY
-- 10/02/1962, 10/2/1962, 9/2/1962, ...
-- monthFirst :: ∀ e. ParserError e => Parser e DateParts

-------------------------
-- Generic Date Parser --
-------------------------

-- 13. Create a parser that can parse both date formats
-- date :: ∀ e. ParserError e => Parser e DateParts

main :: Effect Unit
main = do
  log "Exercise Chapter 19 - Date Parser."
  log $ show $ parse' (atMost' (-2) alphaNum) "a1b2c3" -- (Right (Tuple "a1b2c3" ""))
  log $ show $ parse' (atMost' 2 alphaNum) "$_$"       -- (Right (Tuple "$_$" ""))
  log $ show $ parse' (atMost' 2 alphaNum) "a1b2c3"    -- (Right (Tuple "b2c3" "a1"))
  log $ show $ parse' yearFirst "1962-10-02"
  log $ show $ parse' monthFirst "10/2/1962"
  log $ show $ parse' yearFirst "1999-12-31"
  log $ show $ parse' monthFirst "12/31/1999"
  log $ show $ parse' date "1962-10-02"
  log $ show $ parse' date "10/2/1962"
  log $ show $ parse' date "1999-12-31"
  log $ show $ parse' date "12/31/1999"

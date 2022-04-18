module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Parser

---------------------------
-- Some and Many Parsers --
---------------------------

---------------------------------------------
-- Mutually recursive definition ------------
---------------------------------------------
-- "some" = "one" and "many" (+ in regexs) --
-- "many" = "some" or "none" (* in regexs) --
---------------------------------------------

-- PureScript is a strict language. As both parsers are defined in terms of the other we have to
-- write a Lazy instance for Parser. At least one of "some" or "many" has to be lazy.
-- We'll choose "some" to be lazy.

-- 1. Write "some" and "many" in a general form using Arrays from its definitions above.
-- Do not worry about mutual recursion at this point. Use Applicative parsing.

-- some :: ∀ e a. Parser e a -> Parser e (Array a)
-- many :: ∀ e a. Parser e a -> Parser e (Array a)

-- 2. Create a Lazy instance for Parser.

-- 3. Make "some" lazy by deferring the "many" parser.

-- some :: ∀ e a. Parser e a -> Parser e (Array a)

-- 4. Write specific versions using Char and String.
-- We go from general to specific bc String is a primitive data type for which no functor exists.

-- some' :: ∀ e. Parser e Char -> Parser e String
-- many' :: ∀ e. Parser e Char -> Parser e String

-- 5. Generalize "some" and "many" even more so that they use Unfoldable instead of Array.
-- Pass a cons function parameter bc Unfoldable does not have one.

-- some :: ∀ e a f. Unfoldable f => (a -> f a -> f a) -> Parser e a -> Parser e (f a)
-- many :: ∀ e a f. Unfoldable f => (a -> f a -> f a) -> Parser e a -> Parser e (f a)

-- 6. "some" will never return an empty Unfoldable.
-- Make sure "some" parses at least one character by using NonEmpty.

-- some :: ∀ e a f. Unfoldable f => (a -> f a -> f a) -> Parser e a -> Parser e (NonEmpty f a)
-- many :: ∀ e a f. Unfoldable f => (a -> f a -> f a) -> Parser e a -> Parser e (f a)

-- Hint: The compiler will complain about "many".
-- We have to transform a NonEmpty to something that is not NonEmpty.
-- Use "fromNonEmpty" for it. Look it up on Pursuit.

-- 7. Make the resulting combinators "some" and "many" even more general.
-- Let's use any Monad instead of just our Parser and adapt the Constraints.

-- some :: ∀ a f m. Unfoldable f => (a -> f a -> f a) -> m a -> m (NonEmpty f a)
-- many :: ∀ a f m. Unfoldable f => (a -> f a -> f a) -> m a -> m (f a)

----------------------------------------------
-- Helper Functions Using "some" and "many" --
----------------------------------------------

-- 8. Write a parser called "digits" that parses at least one digit.

-- digits :: ∀ e. ParserError e => Parser e String

-- 9. Write a parser called "uglyA" that parses following regular expression including capturing in do notation (monadic style):
-- (\d{1,4}), ([a-zA-Z ]+)([0-9]*)

-- We capture the capture groups in an array.

-- uglyM :: ∀ e. ParserError e => Parser e (Array String)

-- 10. Write the same parser in applicative style and call it uglyA.

-- uglyA :: ∀ e. ParserError e => Parser e (Array String)

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Exercise Chapter 19 - Some and Many Parsers."
  log $ show $ parse' (some' digit) "2343423423abc"            -- (Right (Tuple "abc" "2343423423"))
  log $ show $ parse' (some' digit) "_2343423423abc"           -- (Left (InvalidChar "digit"))
  log $ show $ parse' (many' digit) "_2343423423abc"           -- (Right (Tuple "_2343423423abc" ""))
  log $ show $ parse' digits "2343423423abc"                   -- (Right (Tuple "abc" "2343423423"))
  log $ show $ parse' digits "_2343423423abc"                  -- (Left (InvalidChar "digit"))
  log $ show $ parse' uglyM "17, some words"                   -- (Right (Tuple "" ["17","some words",""]))
  log $ show $ parse' uglyM "5432, some more words1234567890"  -- (Right (Tuple "" ["5432","some more words","1234567890"]))
  log $ show $ parse' uglyA "17, some words"                   -- (Right (Tuple "" ["17","some words",""]))
  log $ show $ parse' uglyA "5432, some more words1234567890"  -- (Right (Tuple "" ["5432","some more words","1234567890"]))

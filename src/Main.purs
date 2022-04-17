module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

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
-- write a Lazy instance for Parser. One of "some" or "many" will be lazy. We'll choose "some" to be lazy.

-- 1. Write "some", "many" and "none" in a general form using Arrays from its definitions above. Do not worry about mutual recursion at this point. Use Applicative parsing.

-- a) some :: ∀ e a. Parser e a -> Parser e (Array a)

-- b) many :: ∀ e a. Parser e a -> Parser e (Array a)

-- 2. Create a Lazy instance for Parser.

-- Make some lazy by deferring the many parser.

-- 3. Write specific versions using Char and String.
-- We go from general to specific bc String is a primitive data type for which no functor exists.

-- a) some' :: ∀ e. Parser e Char -> Parser e String

-- b) many' :: ∀ e. Parser e Char -> Parser e String

-- 4. Generalize "some" and "many" even more so that they use Unfoldable instead of Array.
-- Pass a cons function parameter bc Unfoldable does not have one.

-- a) some :: ∀ e a f. Unfoldable f => (a -> f a -> f a) -> Parser e a -> Parser e (f a)

-- b) many :: ∀ e a f. Unfoldable f => (a -> f a -> f a) -> Parser e a -> Parser e (f a)

-- 5. "some" will never return an empty Unfoldable.
-- Make sure "some" parses at least one character by using NonEmpty.

-- some :: ∀ e a f. Unfoldable f => (a -> f a -> f a) -> Parser e a -> Parser e (NonEmpty f a)

-- 6. The compiler will complain about "many".
-- We have to transform a NonEmpty to something that is not NonEmpty.

-- Use "fromNonEmpty" for it. Look it up on Pursuit.

-- 7. Make the resulting combinators "some" and "many".
-- Let's use any Monad instead of just our Parser and adapt the Constraints.

-- a) some :: ∀ a f m. Unfoldable f => (a -> f a -> f a) -> m a -> m (NonEmpty f a)

-- b) many :: ∀ a f m. Unfoldable f => (a -> f a -> f a) -> m a -> m (f a)

----------------------------------------------
-- Helper Functions Using "some" and "many" --
----------------------------------------------

-- 8. Write a parser called "digits" that parses at least one digit.

-- digits :: ∀ e. ParserError e => Parser e String

-- 9. Write a parser called "uglyA" that parses following regular expression including capturing in do notation (monadic style):
-- (\d{1,4}), ([a-zA-Z ]+)([0-9]*)

-- We capture the capture groups in an array.
-- uglyM :: ∀ e. Parser e (Array String)

-- Hint: You might have to change constChar and the type signature of uglyM.

-- 10. Write the same parser in applicative style and call it uglyA.

-- uglyA :: ∀ e. Parser e (Array String)

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Exercise Chapter 19 - Some and Many Parsers."
  log $ show $ parse' (some' digit) "2343423423abc"
  log $ show $ parse' (some' digit) "_2343423423abc"
  log $ show $ parse' (many' digit) "_2343423423abc"
  log $ show $ parse' digits "2343423423abc" -- (Right (Tuple "abc" "2343423423"))
  log $ show $ parse' digits "_2343423423abc" -- (Left (InvalidChar "digit"))
  log $ show $ parse' uglyM "17, some words" -- (Right (Tuple "" ["17","some words",""]))
  log $ show $ parse' uglyM "5432, some more words1234567890" -- (Right (Tuple "" ["5432","some more words","1234567890"]))
  log $ show $ parse' uglyA "17, some words" -- (Right (Tuple "" ["17","some words",""]))
  log $ show $ parse' uglyA "5432, some more words1234567890" -- (Right (Tuple "" ["5432","some more words","1234567890"]))

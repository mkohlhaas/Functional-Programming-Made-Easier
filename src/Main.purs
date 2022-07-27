module Main where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Lazy (class Lazy, defer)
import Data.Array ((:))
import Data.NonEmpty (NonEmpty, fromNonEmpty, (:|))
import Data.String.CodeUnits (fromCharArray)
import Data.Unfoldable (class Unfoldable, none)
import Effect (Effect)
import Effect.Console (log)
import Parser (class ParserError, Parser, constChar, constChar', digit, letter, parse', range')

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

-- First we write specialized parsers that will be later expressed from generic ones.
-- E.g specialized `some'` will be expressed from generic `some`.

--------------------------------------------------------------------------------------------
-- 1. Write "some'" and "many'" in a general form using Arrays from its definitions above --
--------------------------------------------------------------------------------------------
-- Do not worry about mutual recursion at this point. It will blow the stack but we'll fix it shortly.

-- some' ∷ ∀ e a. Parser e a → Parser e (Array a)

-- many' ∷ ∀ e a. Parser e a → Parser e (Array a)

------------------------------------------
-- 2. Create a Lazy instance for Parser --
------------------------------------------

----------------------------------------------------------
-- 3. Make "some'" lazy by deferring the "many'" parser --
----------------------------------------------------------

-- some' ∷ ∀ e a. Parser e a → Parser e (Array a)

-- many' ∷ ∀ e a. Parser e a → Parser e (Array a)

----------------------------------------------------
-- 4. Write specific versions for Char and String --
----------------------------------------------------

-- some'' ∷ ∀ e. Parser e Char → Parser e String

-- many'' ∷ ∀ e. Parser e Char → Parser e String

----------------------------------------------------------------------
-- 5. Write a parser called "digits" that parses at least one digit --
----------------------------------------------------------------------

-- digits ∷ ∀ e. ParserError e ⇒ Parser e String

-----------------------------------------------------------------------------------------------------------------
-- 6. Write a parser for the following regular expression (including capturing) in do notation (monadic style) --
-----------------------------------------------------------------------------------------------------------------
-- (\d{1,4}), ([a-zA-Z ]+)([0-9]*)

-- We capture the capture groups in an array.
-- uglyM ∷ ∀ e. ParserError e ⇒ Parser e (Array String)

---------------------------------------------------
-- 7. Write the same parser in applicative style --
---------------------------------------------------

-- uglyA ∷ ∀ e. ParserError e ⇒ Parser e (Array String)

-----------------------------------------------------
-- 8. All tests should work now. Let's generalize! --
-----------------------------------------------------
-- Generalize some and many and express the primed versions with these generic parsers.
-- Comment out all the previous primed versions.

-- Write generic some and many!
-- Pass a cons function parameter because Unfoldable does not have one.

-- some ∷ ∀ e a f. Unfoldable f ⇒ (a → f a → f a) → Parser e a → Parser e (f a)

-- many ∷ ∀ e a f. Unfoldable f ⇒ (a → f a → f a) → Parser e a → Parser e (f a)

-- Write primed versions from the generic versions!
-- some' ∷ ∀ e a. Parser e a → Parser e (Array a)

-- many' ∷ ∀ e a. Parser e a → Parser e (Array a)

-- some'' ∷ ∀ e. Parser e Char → Parser e String

-- many'' ∷ ∀ e. Parser e Char → Parser e String

-----------------------------------------------------
-- 9. `some` will never return an empty Unfoldable --
-----------------------------------------------------
-- The compiler will complain about "many".
-- We have to transform a NonEmpty to something that is not NonEmpty. ;-)
-- Use `fromNonEmpty` to fix the issue. Look it up on Pursuit.
-- Write new primed versions. Comment out their previous versions.

-- some ∷ ∀ e a f. Unfoldable f ⇒ (a → f a → f a) → Parser e a → Parser e (NonEmpty f a)

-- many ∷ ∀ e a f. Unfoldable f ⇒ (a → f a → f a) → Parser e a → Parser e (f a)

-- some' ∷ ∀ e a. Parser e a → Parser e (Array a)

-- many' ∷ ∀ e a. Parser e a → Parser e (Array a)

-- some'' ∷ ∀ e. Parser e Char → Parser e String

-- many'' ∷ ∀ e. Parser e Char → Parser e String

-----------------------------------------------------------------------------
-- 10. Make the resulting combinators "some" and "many" even more general! --
-----------------------------------------------------------------------------
-- Let's use any Monad instead of just our Parser and adapt the constraints.
-- Comment out their previous versions. Leave the primed versions alone.

-- some ∷ ∀ a f m. Unfoldable f ⇒ (a → f a → f a) → m a → m (NonEmpty f a)
-- some cons p = (:|) <$> p <*> defer (\_ → many cons p)

-- many ∷ ∀ a f m. Unfoldable f ⇒ (a → f a → f a) → m a → m (f a)
-- many cons p = fromNonEmpty cons <$> some cons p <|> pure none

----------
-- Main --
----------

main ∷ Effect Unit
main = do
  log "Exercise Chapter 19 - Some and Many Parsers."
  log $ show $ parse' (some' digit) "2343423423abc" ------------- (Right (Tuple "abc" ['2','3','4','3','4','2','3','4','2','3']))
  log $ show $ parse' (some' digit) "_2343423423abc" ------------ (Left (InvalidChar "digit"))
  log $ show $ parse' (many' digit) "_2343423423abc" ------------ (Right (Tuple "_2343423423abc" []))
  log $ show $ parse' (some'' digit) "2343423423abc" ------------ (Right (Tuple "abc" "2343423423"))
  log $ show $ parse' (some'' digit) "_2343423423abc"------------ (Left (InvalidChar "digit"))
  log $ show $ parse' (many'' digit) "_2343423423abc" ----------- (Right (Tuple "_2343423423abc" ""))
  log $ show $ parse' digits "2343423423abc" -------------------- (Right (Tuple "abc" "2343423423"))
  log $ show $ parse' digits "_2343423423abc" ------------------- (Left (InvalidChar "digit"))
  log $ show $ parse' uglyM "17, some words" -------------------- (Right (Tuple "" ["17","some words",""]))
  log $ show $ parse' uglyM "5432, some more words1234567890" --- (Right (Tuple "" ["5432","some more words","1234567890"]))
  log $ show $ parse' uglyA "17, some words" -------------------- (Right (Tuple "" ["17","some words",""]))
  log $ show $ parse' uglyA "5432, some more words1234567890" --- (Right (Tuple "" ["5432","some more words","1234567890"]))

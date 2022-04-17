module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

---------------------------
-- Some and Many Parsers --
---------------------------

-------------------------
-- some = one and many --
-- many = some or none --
-------------------------

-- PureScript is a strict language. As both parsers are defined in terms of the other we have to
-- write a Lazy instance for Parser. (One of some or many will be lazy. We'll choose some to be lazy.)

-- 1. Write some, many and none in a general form using Arrays
-- 2. Write a specific version using Char and String; (we go from general to specific bc String is a primitive data type for which no functor exists)
-- 3. Generalize even more so that they use Unfoldable instead of Array. Pass a cons function parameter bc Unfoldable does not have one.

-- We don't have a one parser. Make sure some parses at least one character by using NonEmpty.

-- [Make the resulting combinators some and many as general as possible by replacing Parser with a type constructor] ???

------------------------------------------
-- Helper Functions Using some and many --
------------------------------------------

-- 4. Write a parser called digits that parses digits (at least one); parser result is a String not an Int

-------------------------
-- Using some and many --
-------------------------

-- 5. Write a parser called ugly that parses following regular expression including capturing:
-- (\d{1,4}), ([a-zA-Z ]+)([0-9]*)

-- [6. Write the same parser in applicative style and call it uglyA] ???

-- [7. Write the same parser with bind (>>=) and call it uglyB] ???

main :: Effect Unit
main = do
  log "Exercise Chapter 19 - Some and Many Parsers."
  log $ show $ parse' digits "2343423423abc" -- (Right (Tuple "abc" "2343423423"))
  log $ show $ parse' (many' digit) "_2343423423abc" -- (Right (Tuple "_2343423423abc" ""))
  log $ show $ parse' digits "_2343423423abc" -- (Left (InvalidChar "digit"))
  log $ show $ parse' ugly "17, some words" -- (Right (Tuple "" ["17","some words",""]))
  log $ show $ parse' ugly "5432, some more words1234567890" -- (Right (Tuple "" ["5432","some more words","1234567890"]))

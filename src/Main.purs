module Main where

import Prelude
import Control.Alt (class Alt, (<|>))
import Data.Array as A
import Data.CodePoint.Unicode (isDecDigit, isAlpha)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (uncons, fromCharArray)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable as U
import Effect (Effect)
import Effect.Console (log)

-- The Parsing State is going to need to be passed from Parser to Parser, i.e. when the current Parser is done,
-- it passes what's left of the String to the next Parser who takes a stab at parsing what's left. Also, if a
-- single Parser in the chain were to fail, we want to short-circuit the parsing and return the error, hopefully
-- with some useful information as to what went wrong.

------------------------------
-- Data Types and Type Classes
------------------------------

class ParserError (e :: Type) where
  eof :: e
  invalidChar :: String -> e

data PError
  = EOF
  | InvalidChar String

type ParserState a = Tuple String a

type ParseFunction e a = ParserError e => String -> Either e (ParserState a)

newtype Parser e a = Parser (ParseFunction e a)

instance functorParser :: Functor (Parser e) where
  map f g = Parser \s -> map f <$> parse g s

instance applyParser :: Apply (Parser e) where
  apply = ap

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

parse :: ∀ e a. Parser e a -> ParseFunction e a
parse (Parser f) = f

derive instance genericPError :: Generic PError _

instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF
  invalidChar s = InvalidChar s

char :: ∀ e. Parser e Char
char =
  Parser \s -> case uncons s of
    Nothing -> Left eof
    Just { head, tail } -> Right $ Tuple tail head

parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' = parse

count :: ∀ e a. Int -> Parser e a -> Parser e (Array a)
count n p
  | n < 0 = pure []
  | otherwise = sequence (A.replicate n p)

count' :: ∀ e a f. Traversable f => U.Unfoldable f => Int -> Parser e a -> Parser e (f a)
count' n p
  | n < 0 = pure U.none
  | otherwise = sequence (U.replicate n p)

count'' :: ∀ e. Int -> Parser e Char -> Parser e String
count'' n p = fromCharArray <$> count n p

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

----------------------
-- Helper Functions --
----------------------

atMost :: ∀ e f a. U.Unfoldable f => (a -> f a -> f a) -> Int -> Parser e a -> Parser e (f a)
atMost cons n p
  | n <= 0 = pure U.none
  | otherwise = optional U.none $ p >>= \c -> cons c <$> atMost cons (n - 1) p

atMost' :: ∀ e. Int -> Parser e Char -> Parser e String
atMost' n p = fromCharArray <$> atMost A.cons n p

optional :: ∀ e a. a -> Parser e a -> Parser e a
optional x p = p <|> pure x

range :: ∀ e f a. Semigroup (f a) => Traversable f => U.Unfoldable f => (a -> f a -> f a) -> Int -> Int -> Parser e a -> Parser e (f a)
range cons min max p
  | min > max, min < 0, max <= 0 = pure U.none
  | otherwise = count' min p >>= \cs -> (cs <> _) <$> atMost cons (max - min) p

range' :: ∀ e. Int -> Int -> Parser e Char -> Parser e String
range' min max p = fromCharArray <$> range A.cons min max p

constChar :: ∀ e. ParserError e => Char -> Parser e Unit
constChar = void <<< constChar'

constChar' :: ∀ e. ParserError e => Char -> Parser e Char
constChar' c = satisfy (show c) (_ == c)

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
  log "Ch. 19 Some and Many Parsers."
-- log $ show $ parse' digits "2343423423abc"                  -- (Right (Tuple "abc" "2343423423"))
-- log $ show $ parse' (many' digit) "_2343423423abc"          -- (Right (Tuple "_2343423423abc" ""))
-- log $ show $ parse' digits "_2343423423abc"                 -- (Left (InvalidChar "digit"))
-- log $ show $ parse' ugly "17, some words"                   -- (Right (Tuple "" ["17","some words",""]))
-- log $ show $ parse' ugly "5432, some more words1234567890"  -- (Right (Tuple "" ["5432","some more words","1234567890"]))

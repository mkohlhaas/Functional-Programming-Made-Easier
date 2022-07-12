module Parser where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Lazy (class Lazy, defer)
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

-- The Parsing State is going to need to be passed from Parser to Parser, i.e. when the current Parser is done,
-- it passes what's left of the String to the next Parser who takes a stab at parsing what's left. Also, if a
-- single Parser in the chain were to fail, we want to short-circuit the parsing and return the error, hopefully
-- with some useful information as to what went wrong.

------------------------------
-- Data Types and Type Classes
------------------------------

class ParserError e where
  eof :: e
  invalidChar :: String -> e

data PError
  = EOF
  | InvalidChar String

type ParserState a = Tuple String a

type ParseFunction e a = ParserError e => String -> Either e (ParserState a)

newtype Parser e a = Parser (ParseFunction e a)

instance Functor (Parser e) where
  map f g = Parser \s -> map f <$> parse g s

instance Apply (Parser e) where
  apply = ap

instance Applicative (Parser e) where
  pure a = Parser \s -> Right $ Tuple s a

instance Bind (Parser e) where
  bind p f =
    Parser \s -> do
      Tuple s1 x <- parse p s
      parse (f x) s1

instance Monad (Parser e)

instance Alt (Parser e) where
  alt p1 p2 = Parser \s -> parse p1 s <|> parse p2 s

instance Lazy (Parser e a) where
  defer f = Parser \str -> parse (f unit) str

parse :: ∀ e a. Parser e a -> ParseFunction e a
parse (Parser f) = f

derive instance Generic PError _

instance Show PError where
  show = genericShow

instance ParserError PError where
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

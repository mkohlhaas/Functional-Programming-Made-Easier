module Parser where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.CodePoint.Unicode (isDecDigit, isAlpha)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (uncons, fromCharArray)
import Data.Traversable (class Traversable)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, replicateA)

---------------------------------
-- Data Types and Type Classes --
---------------------------------

class ParserError e where
  eof ∷ e
  invalidChar ∷ String → e

data PError = EOF | InvalidChar String
type ParserState a = Tuple String a
type ParseFunction e a = ParserError e ⇒ String → Either e (ParserState a)
newtype Parser e a = Parser (ParseFunction e a)

data Threeple a b c = Threeple a b c

derive instance Generic (Threeple a b c) _

instance Show (Parser e a) where
  show _ = "Parser ..."

instance (Show a, Show b, Show c) ⇒ Show (Threeple a b c) where
  show = genericShow

derive instance Generic PError _

instance Show PError where
  show = genericShow

instance ParserError PError where
  eof = EOF
  invalidChar s = InvalidChar s

instance Functor (Parser e) where
  map f g = Parser \s → map f <$> parse g s

instance Apply (Parser e) where
  apply = ap

instance Applicative (Parser e) where
  pure a = Parser \s → Right $ Tuple s a

instance Bind (Parser e) where
  bind p f = Parser \s → do
    Tuple s1 x <- parse p s
    parse (f x) s1

instance Monad (Parser e)

instance Alt (Parser e) where
  alt p1 p2 = Parser \s → parse p1 s <|> parse p2 s

---------------------------------
-- Helper Functions for Parser --
---------------------------------

parse ∷ ∀ e a. Parser e a → ParseFunction e a
parse (Parser f) = f

parse' ∷ ∀ a. Parser PError a → ParseFunction PError a
parse' = parse

char ∷ ∀ e. Parser e Char
char = Parser \s → case uncons s of
  Nothing → Left eof
  Just { head, tail } → Right $ Tuple tail head

count ∷ ∀ e a f. Traversable f ⇒ Unfoldable f ⇒ Int → Parser e a → Parser e (f a)
count = replicateA

count' ∷ ∀ e. Int → Parser e Char → Parser e String
count' n p = fromCharArray <$> count n p

fail ∷ ∀ e a. ParserError e ⇒ e → Parser e a
fail e = Parser $ const $ Left e

satisfy ∷ ∀ e. ParserError e ⇒ String → (Char → Boolean) → Parser e Char
satisfy expected pred = char >>= \c → if pred c then pure c else fail $ invalidChar expected

digit ∷ ∀ e. ParserError e ⇒ Parser e Char
digit = satisfy "digit" $ isDecDigit <<< codePointFromChar

letter ∷ ∀ e. ParserError e ⇒ Parser e Char
letter = satisfy "letter" $ isAlpha <<< codePointFromChar

alphaNum ∷ ∀ e. ParserError e ⇒ Parser e Char
alphaNum = letter <|> digit <|> fail (invalidChar "alphaNum")

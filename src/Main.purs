module Main where

import Prelude

import Data.Array as A
import Data.CodePoint.Unicode (isAlpha, isDecDigit)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (fromCharArray, uncons)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, replicateA)
import Effect (Effect)
import Effect.Console (log)

-- The Parsing State is going to need to be passed from Parser to Parser, i.e. when the current Parser is done,
-- it passes what’s left of the String to the next Parser who takes a stab at parsing what's left. Also, if a
-- single Parser in the chain were to fail, we want to short-circuit the parsing and return the error with
-- some useful information as to what went wrong.

---------------------------------
-- Data Types and Type Classes --
---------------------------------

-- e = error type
-- a = return type

class ParserError e where
  eof ∷ e
  invalidChar ∷ String → e

data PError = EOF | InvalidChar String -- application specific parse error type
type ParserState a = Tuple String a -- left-over string and parsed value
type ParseFunction e a = ParserError e ⇒ String → Either e (ParserState a)
newtype Parser e a = Parser (ParseFunction e a)
data Threeple a b c = Threeple a b c

instance Functor (Parser e) where
  map f g = Parser \s → map f <$> parse g s

instance applyParser ∷ Apply (Parser e) where
  apply f g = Parser \s → case parse f s of
    Left e → Left e
    Right (Tuple s1 h) → case parse g s1 of
      Left e → Left e
      Right (Tuple s2 a) → Right $ Tuple s2 $ h a

instance Applicative (Parser e) where
  pure a = Parser \s → Right $ Tuple s a

-- 1. Create a Bind instance for Parser.
-- 2. Create a Monad instance for Parser and rewrite Apply (Parser e) in do notation.
-- 3. Create Alt instance for Parser.

------------
-- Parser --
------------

parse ∷ ∀ e a. Parser e a → ParseFunction e a
parse (Parser f) = f

parse' ∷ ∀ a. Parser PError a → ParseFunction PError a
parse' = parse

derive instance genericPError ∷ Generic PError _
instance showPError ∷ Show PError where
  show = genericShow

derive instance genericThreeple ∷ Generic (Threeple a b c) _
instance showThreeple ∷ (Show a, Show b, Show c) ⇒ Show (Threeple a b c) where
  show = genericShow

char ∷ ∀ e. Parser e Char
char = Parser \s → case uncons s of
  Nothing → Left eof
  Just { head, tail } → Right $ Tuple tail head

-- 4. These are our applicative parsers. Rewrite them using our new monadic parser using do notation.
--    Replace in new function name A with B for bind, e.g. twoCharsA → twoCharsB.

twoCharsA ∷ ∀ e. Parser e (Tuple Char Char)
twoCharsA = Tuple <$> char <*> char

threeCharsA ∷ ∀ e. Parser e (Tuple Char (Tuple Char Char))
threeCharsA = Tuple <$> char <*> twoCharsA

threeCharsA' ∷ ∀ e. Parser e (Threeple Char Char Char)
threeCharsA' = Threeple <$> char <*> char <*> char

threeCharsA'' ∷ ∀ e. Parser e String
threeCharsA'' = (\c1 c2 c3 → fromCharArray [ c1, c2, c3 ]) <$> char <*> char <*> char

tenCharsA ∷ ∀ e. Parser e String
tenCharsA = (\c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 → fromCharArray [ c1, c2, c3, c4, c5, c6, c7, c8, c9, c10 ])
  <$> char
  <*> char
  <*> char
  <*> char
  <*> char
  <*> char
  <*> char
  <*> char
  <*> char
  <*> char

-- 5. Write a parser that always fails.
-- fail ∷ ∀ e a. ParserError e ⇒ e → Parser e a

-- 6. Write a satisfy function (the first argument - the String - is an error message).
-- satisfy ∷ ∀ e. ParserError e ⇒ String → (Char → Boolean) → Parser e Char

-- 7. Write a Char-parser called digit parser based on satisfy using isDecDigit.
-- digit ∷ ∀ e. ParserError e ⇒ Parser e Char

-- 8. Write a Char-parser called letter parser (use isAlpha).
-- letter ∷ ∀ e. ParserError e ⇒ Parser e Char

-- 9. Write an alphanum parser using the digit and letter parsers. Make sure it provides a meaningful error message.
-- alphaNum ∷ ∀ e. ParserError e ⇒ Parser e Char

count ∷ ∀ m f a. Applicative m ⇒ Unfoldable f ⇒ Traversable f ⇒ Int → m a → m (f a)
count = replicateA

-- 10. Refactor count to make it more pleasant to work with and call it count'.

main ∷ Effect Unit
main = do
  log "Exercisse Chapter 19 - Monadic Parser."
  log "-------------------------"
  log "-- Applicative Parsers --"
  log "-------------------------"
  log $ show $ (parse char "ABC" ∷ Either PError _) ------------------------------------ (Right (Tuple "BC" 'A')).
  log $ show $ (parse twoCharsA "ABC" ∷ Either PError _) ------------------------------- (Right (Tuple "C" (Tuple 'A' 'B'))).
  log $ show $ (parse threeCharsA "ABC" ∷ Either PError _) ----------------------------- (Right (Tuple "" (Tuple 'A' (Tuple 'B' 'C'))))
  log $ show $ (parse threeCharsA' "ABC" ∷ Either PError _) ---------------------------- (Right (Tuple "" (Threeple 'A' 'B' 'C')))
  log $ show $ (parse threeCharsA'' "ABC" ∷ Either PError _) --------------------------- (Right (Tuple "" "ABC"))
  log $ show $ parse' char "ABC" ------------------------------------------------------- (Right (Tuple "BC" 'A')).
  log $ show $ parse' twoCharsA "ABC" -------------------------------------------------- (Right (Tuple "C" (Tuple 'A' 'B'))).
  log $ show $ parse' threeCharsA "ABC" ------------------------------------------------ (Right (Tuple "" (Tuple 'A' (Tuple 'B' 'C'))))
  log $ show $ parse' threeCharsA' "ABC" ----------------------------------------------- (Right (Tuple "" (Threeple 'A' 'B' 'C')))
  log $ show $ parse' threeCharsA'' "ABC" ---------------------------------------------- (Right (Tuple "" "ABC"))
  log $ show $ parse' threeCharsA "A" -------------------------------------------------- (Left EOF)
  log $ show $ parse' tenCharsA "ABCDEFGHIJKLMNOPQRSTUVXYZ" ---------------------------- (Right (Tuple "KLMNOPQRSTUVXYZ" "ABCDEFGHIJ"))
  log "---------------------"
  log "-- Monadic Parsers --"
  log "---------------------"
  log $ show $ (parse char "ABC" ∷ Either PError _) ------------------------------------ (Right (Tuple "BC" 'A')).
  log $ show $ (parse twoCharsB "ABC" ∷ Either PError _) ------------------------------- (Right (Tuple "C" (Tuple 'A' 'B'))).
  log $ show $ (parse threeCharsB "ABC" ∷ Either PError _) ----------------------------- (Right (Tuple "" (Tuple 'A' (Tuple 'B' 'C'))))
  log $ show $ (parse threeCharsB' "ABC" ∷ Either PError _) ---------------------------- (Right (Tuple "" (Threeple 'A' 'B' 'C')))
  log $ show $ (parse threeCharsB'' "ABC" ∷ Either PError _) --------------------------- (Right (Tuple "" "ABC"))
  log $ show $ parse' char "ABC" ------------------------------------------------------- (Right (Tuple "BC" 'A')).
  log $ show $ parse' twoCharsB "ABC" -------------------------------------------------- (Right (Tuple "C" (Tuple 'A' 'B'))).
  log $ show $ parse' threeCharsB "ABC" ------------------------------------------------ (Right (Tuple "" (Tuple 'A' (Tuple 'B' 'C'))))
  log $ show $ parse' threeCharsB' "ABC" ----------------------------------------------- (Right (Tuple "" (Threeple 'A' 'B' 'C')))
  log $ show $ parse' threeCharsB'' "ABC" ---------------------------------------------- (Right (Tuple "" "ABC"))
  log $ show $ parse' threeCharsB "A" -------------------------------------------------- (Left EOF)
  log $ show $ parse' tenCharsB "ABCDEFGHIJKLMNOPQRSTUVXYZ" ---------------------------- (Right (Tuple "KLMNOPQRSTUVXYZ" "ABCDEFGHIJ"))
  log "----------------------"
  log "-- Helper Functions --"
  log "----------------------"
  log $ show $ parse' (fromCharArray <$> (count 10 char)) "ABCDEFGHIJKLMNOPQRSTUVXYZ" -- (Right (Tuple "KLMNOPQRSTUVXYZ" "ABCDEFGHIJ"))
  log $ show $ parse' (count' 3 digit) "123456" ---------------------------------------- (Right (Tuple "456" "123"))
  log $ show $ parse' (count' 3 digit) "abc456" ---------------------------------------- (Left (InvalidChar "digit"))
  log $ show $ parse' (count' 4 letter) "Freddy" --------------------------------------- (Right (Tuple "dy" "Fred"))
  log $ show $ parse' (count' 10 alphaNum) "a1b2c3d4e5" -------------------------------- (Right (Tuple "" "a1b2c3d4e5"))
  log $ show $ parse' (count' 10 alphaNum) "######" ------------------------------------ (Left (InvalidChar "alphaNum"))

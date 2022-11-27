module Main where

import Prelude

import Data.Either (Either(..))
import Data.String.CodeUnits (fromCharArray, uncons)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

-- The Parsing State needs to be passed from Parser to Parser, i.e. when the current Parser is done,
-- it passes what's left of the String to the next Parser. Also, if a single Parser in the chain were to fail,
-- we want to short-circuit the parsing and return the error.

---------------------------------
-- Data Types and Type Classes --
---------------------------------

-- e = error type
-- a = return type

class ParserError e where
  eof ∷ e

data PError = EOF -- application specific parse error type
type ParserState a = Tuple String a -- left-over string and parsed value
type ParseFunction e a = ParserError e ⇒ String → Either e (ParserState a)
newtype Parser e a = Parser (ParseFunction e a)
data Threeple a b c = Threeple a b c

-------------------------------------------
-- Create Necessary Instances for Parser --
-------------------------------------------

----------------------
-- Using the Parser --
----------------------

-- Write a parse function
-- parse ∷ ∀ e a. Parser e a → ParseFunction e a

-- Use parse in map/apply

-- Create Show instance for PError

-- Create ParserError instance for PError

-- Write a char parser using String libary function uncons
-- char ∷ ∀ e. Parser e Char

-- Write a two-char parser
-- twoChars ∷ ∀ e. Parser e (Tuple Char Char)

-- Write a char-two-char parser
-- threeChars ∷ ∀ e. Parser e (Tuple Char (Tuple Char Char))

-- Write a Show instance for Threeple

-- Write a Threeple 3-char parser
-- threeChars' ∷ ∀ e. Parser e (Threeple Char Char Char)

-- Write a 3-char parser returning a String using library function fromCharArray
-- threeChars'' ∷ ∀ e. Parser e String

-- Write a parse' function which includes the error type in its signature
-- parse' ∷ ∀ a. Parser PError a → ParseFunction PError a

-- Write a 10-char parser in the same manner as the 3-char parser
-- tenChars ∷ ∀ e. Parser e String

-- Do the same using sequence from Data.Traversable and replicate from Data.Array using this helper function
-- count ∷ ∀ e a. Int → Parser e a → Parser e (Array a)

-- Make count more generic and call it count'

main ∷ Effect Unit
main = do
  log "Exercise Ch. 17 Applicative Parser."
  log $ show $ (parse char "ABC" ∷ Either PError _) ----------------------------------------- (Right (Tuple "BC" 'A')).
  log $ show $ (parse twoChars "ABC" ∷ Either PError _) ------------------------------------- (Right (Tuple "C" (Tuple 'A' 'B'))).
  log $ show $ (parse threeChars "ABC" ∷ Either PError _) ----------------------------------- (Right (Tuple "" (Tuple 'A' (Tuple 'B' 'C'))))
  log $ show $ (parse threeChars' "ABC" ∷ Either PError _) ---------------------------------- (Right (Tuple "" (Threeple 'A' 'B' 'C')))
  log $ show $ (parse threeChars'' "ABC" ∷ Either PError _) --------------------------------- (Right (Tuple "" "ABC"))
  log $ show $ parse' char "ABC" ------------------------------------------------------------ (Right (Tuple "BC" 'A')).
  log $ show $ parse' twoChars "ABC" -------------------------------------------------------- (Right (Tuple "C" (Tuple 'A' 'B'))).
  log $ show $ parse' threeChars "ABC" ------------------------------------------------------ (Right (Tuple "" (Tuple 'A' (Tuple 'B' 'C'))))
  log $ show $ parse' threeChars' "ABC" ----------------------------------------------------- (Right (Tuple "" (Threeple 'A' 'B' 'C')))
  log $ show $ parse' threeChars'' "ABC" ---------------------------------------------------- (Right (Tuple "" "ABC"))
  log $ show $ parse' threeChars "A" -------------------------------------------------------- (Left EOF)
  log $ show $ parse' tenChars "ABCDEFGHIJKLMNOPQRSTUVXYZ" ---------------------------------- (Right (Tuple "KLMNOPQRSTUVXYZ" "ABCDEFGHIJ"))
  log $ show $ parse' (fromCharArray <$> (count 10 char)) "ABCDEFGHIJKLMNOPQRSTUVXYZ" ------- (Right (Tuple "KLMNOPQRSTUVXYZ" "ABCDEFGHIJ"))
  log $ show $ parse' (fromCharArray <$> (count' 10 char)) "ABCDEFGHIJKLMNOPQRSTUVXYZ" ------ (Right (Tuple "KLMNOPQRSTUVXYZ" "ABCDEFGHIJ"))
  log $ show $ parse' (fromCharArray <$> (count' (-10) char)) "ABCDEFGHIJKLMNOPQRSTUVXYZ" --- (Right (Tuple "ABCDEFGHIJKLMNOPQRSTUVXYZ" ""))

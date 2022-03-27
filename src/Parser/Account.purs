module Parser.Account where

import Prelude

import Control.Alt ((<|>))
import Data.Array (catMaybes, fromFoldable, many, some, (:))
import Data.CodePoint.Unicode (isLower)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (fromCharArray)
import Entity.Account (Account(..))
import Text.Parsing.Parser (ParserT, fail)
import Text.Parsing.Parser.Combinators (sepBy, (<?>))
import Text.Parsing.Parser.String (string, satisfy, char)
import Text.Parsing.Parser.String.Basic (letter, alphaNum, hexDigit, upper)

type AccountParserT a = ParserT String Identity a

userName :: AccountParserT String
userName = do
  l <- letter
  a <- many alphaNum
  pure $ fromCharArray $ l : a

type PasswordParserT a = ParserT String Identity a

hex :: AccountParserT String
hex = fromCharArray <$> some hexDigit

passwordHash :: AccountParserT String
passwordHash = hex

boolean :: AccountParserT Boolean
boolean =
  string "true" *> pure true
    <|> string "false" *> pure false
    <|> fail "Invalid Boolean"

temporaryPassword :: AccountParserT Boolean
temporaryPassword = boolean

admin :: AccountParserT Boolean
admin = boolean

satisfyCP :: forall m. Monad m => (CodePoint -> Boolean) -> ParserT String m Char
satisfyCP p = satisfy $ p <<< codePointFromChar

lower :: forall m. Monad m => ParserT String m Char
lower = satisfyCP isLower <?> "lowercase letter"

properName :: AccountParserT String
properName = do
  firstLetter <- upper
  taiLetters <- many lower
  pure $ fromCharArray $ firstLetter : taiLetters

firstName :: AccountParserT String
firstName = properName

lastName :: AccountParserT String
lastName = properName

accountParser :: AccountParserT Account
accountParser = do
  userName' <- userName # comma
  passwordHash' <- passwordHash # comma
  temporaryPassword' <- temporaryPassword # comma
  admin' <- admin # comma
  firstName' <- firstName # comma
  lastName' <- lastName
  pure $ Account
    { userName: userName'
    , passwordHash: passwordHash'
    , temporaryPassword: temporaryPassword'
    , admin: admin'
    , firstName: firstName'
    , lastName: lastName'
    }
  where
  comma :: ∀ a. AccountParserT a -> AccountParserT a
  comma p = p <* char ','

accountsParser :: AccountParserT (Array Account)
accountsParser = (Just <$> accountParser <|> pure Nothing) `sepBy` newline <#> catMaybes <<< fromFoldable
  where
  newline = char '\n'

module Handler.Account where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (Either, isLeft)
import Data.Newtype (unwrap)
import Data.String.Utils (lines)
import Data.Traversable (sequence)
import Effect.Aff (Aff)
import Entity.Account (Account)
import Node.Encoding (Encoding(ASCII))
import Node.FS.Aff (exists, readTextFile, writeTextFile)
import Parser.Account (accountParser)
import Text.Parsing.Parser (ParseError, runParserT)

-----------------
-- Helper Data --
-----------------
accountsFile :: String
accountsFile = "accounts.csv"

bootstrapAccount :: String
bootstrapAccount = "admin,placeholder,true,true,Joe,Admin"

loadAccounts :: Aff (Either ParseError (Array Account))
loadAccounts = do
  exists <- try $ exists accountsFile
  when (isLeft exists) $ writeTextFile ASCII accountsFile bootstrapAccount
  accountLines <- lines <$> readTextFile ASCII accountsFile
  pure $ sequence $ unwrap <<< flip runParserT accountParser <$> accountLines

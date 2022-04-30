module Handler.Account where

import Prelude

import Control.Monad.Error.Class (try)
import Crypto (passwordHashHex)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Entity.Account (Account(..))
import Node.Encoding (Encoding(ASCII))
import Node.FS.Aff (exists, readTextFile, writeTextFile, appendTextFile)
import Parser.Account (accountsParser)
import Text.Parsing.Parser (ParseError, runParserT)

----------------
-- Data Types --
----------------
data CreateAccountError = CreateAccountFileError String

-----------------
-- Helper Data --
-----------------
accountsFile :: String
accountsFile = "accounts.csv"

bootstrapAccount :: Aff String
bootstrapAccount = do
  let
    userName = "admin"
    password = "admin"
  passwordHash <- passwordHashHex userName password
  pure $ accountToCSV $ Account
    { userName
    , passwordHash
    , temporaryPassword: true
    , admin: true
    , firstName: "Joe"
    , lastName: "Admin"
    }

----------------------
-- Helper Functions --
----------------------
accountToCSV :: Account -> String
accountToCSV (Account { userName, passwordHash, temporaryPassword, admin, firstName, lastName }) =
  intercalate "," [ userName, passwordHash, show temporaryPassword, show admin, firstName, lastName ] <> "\n"

createAccount :: Account -> Aff (Either CreateAccountError Unit)
createAccount account = lmap show <$> (try $ appendTextFile ASCII accountsFile $ accountToCSV account) <#> lmap CreateAccountFileError

----------------------------
-- Module's Main Function --
----------------------------
loadAccounts :: Aff (Either ParseError (Array Account))
loadAccounts = do
  exists' <- try $ exists accountsFile
  let
    exists = case exists' of
      Left _ -> false
      Right trueOrfalse -> trueOrfalse
  unless exists do
    bootstrapAccount' <- bootstrapAccount
    writeTextFile ASCII accountsFile bootstrapAccount'
  fileData <- readTextFile ASCII accountsFile
  pure $ unwrap $ runParserT fileData accountsParser

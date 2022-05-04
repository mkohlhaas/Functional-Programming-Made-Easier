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

data CreateAccountError = CreateAccountFileError String
type AccountsFileName = String
type CSVString = String

accountsFile :: AccountsFileName
accountsFile = "accounts.csv"

bootstrapAccount :: Aff CSVString
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

accountToCSV :: Account -> CSVString
accountToCSV (Account { userName, passwordHash, temporaryPassword, admin, firstName, lastName }) =
  intercalate "," [ userName, passwordHash, show temporaryPassword, show admin, firstName, lastName ] <> "\n"

createAccount :: Account -> Aff (Either CreateAccountError Unit)
createAccount account = lmap show <$> (try $ appendTextFile ASCII accountsFile $ accountToCSV account) <#> lmap CreateAccountFileError

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

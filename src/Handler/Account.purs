module Handler.Account where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Bifunctor (lmap)
import Data.Foldable (intercalate)
import Data.Either (Either, isLeft)
import Data.Newtype (unwrap)
import Data.String.Utils (lines)
import Data.Traversable (sequence)
import Effect.Aff (Aff)
import Entity.Account (Account(..))
import Node.Encoding (Encoding(ASCII))
import Node.FS.Aff (exists, readTextFile, writeTextFile, appendTextFile)
import Parser.Account (accountParser)
import Text.Parsing.Parser (ParseError, runParserT)

-----------------
-- Helper Data --
-----------------

accountsFile :: String
accountsFile = "accounts.csv"

bootstrapAccount :: String
bootstrapAccount = "admin,placeholder,true,true,Joe,Admin"

----------------------
-- Helper Functions --
----------------------

accountToCSV :: Account -> String
accountToCSV (Account { userName, passwordHash, temporaryPassword, admin, firstName, lastName }) =
  intercalate "," [ userName, passwordHash, show temporaryPassword, show admin, firstName, lastName ]

createAccount :: Account -> Aff (Either String Unit)
createAccount acc = lmap show <$> (try $ appendTextFile ASCII accountsFile $ accountToCSV acc)

----------------------------
-- Module's Main Function --
----------------------------

loadAccounts :: Aff (Either ParseError (Array Account))
loadAccounts = do
  exists <- try $ exists accountsFile
  when (isLeft exists) $ writeTextFile ASCII accountsFile bootstrapAccount
  accountLines <- lines <$> readTextFile ASCII accountsFile
  pure $ sequence $ unwrap <<< flip runParserT accountParser <$> accountLines

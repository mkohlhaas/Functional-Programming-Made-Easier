module Handler.Account where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Bifunctor (lmap)
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (foldl, intercalate)
import Data.Newtype (unwrap)
import Data.String (length)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Utils (lines)
import Data.Traversable (sequence)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Entity.Account (Account(..))
import Node.Buffer (fromString, toString)
import Node.Crypto.Hash (createHash, update, digest)
import Node.Encoding (Encoding(ASCII, UTF8, Hex))
import Node.FS.Aff (exists, readTextFile, writeTextFile, appendTextFile)
import Parser.Account (accountParser)
import Random.LCG (Seed, mkSeed)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (sample)
import Text.Parsing.Parser (ParseError, runParserT)

-----------------
-- Helper Data --
-----------------

accountsFile :: String
accountsFile = "accounts.csv"

bootstrapAccount :: Aff String
bootstrapAccount = do
  pw <- passwordHashHex "admin" "admin"
  let true' = show true
  pure $ intercalate "," [ "admin", pw, true', true', "Joe", "Admin" ]

----------------------
-- Helper Functions --
----------------------

accountToCSV :: Account -> String
accountToCSV (Account { userName, passwordHash, temporaryPassword, admin, firstName, lastName }) =
  intercalate "," [ userName, passwordHash, show temporaryPassword, show admin, firstName, lastName ]

createAccount :: Account -> Aff (Either String Unit)
createAccount acc = lmap show <$> (try $ appendTextFile ASCII accountsFile $ accountToCSV acc)

userNameSeed :: String -> Seed
userNameSeed userName = userName # toCharArray <#> toCharCode # foldl (*) 1 # mkSeed

userNameSalt :: Int -> String -> String
userNameSalt saltLength userName = fromCharArray $ sample (userNameSeed userName) saltLength arbitrary

passwordHashHex :: String -> String -> Aff String
passwordHashHex userName password = do
  let salt = userNameSalt (3 * length userName) userName
  buf <- liftEffect $ fromString (password <> salt) UTF8
  liftEffect $ createHash "sha512" >>= update buf >>= digest >>= toString Hex

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
  accountLines <- lines <$> readTextFile ASCII accountsFile
  pure $ sequence $ unwrap <<< flip runParserT accountParser <$> accountLines

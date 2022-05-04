module Manager.Account where

import Prelude

import Crypto (passwordHashHex)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Map (Map, values)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar, take)
import Effect.Aff.AVar as AVar
import Entity.Account (Account(..))
import Utils (withAVar)

type UserName = String
type Password = String
type Accounts = Map UserName Account

data CreateAccountError = AccountAlreadyExists

startup :: Array Account -> Aff (AVar Accounts)
startup accounts = (accounts <#> \account@(Account { userName }) -> Tuple userName account) # Map.fromFoldable # AVar.new

shutdown :: AVar Accounts -> Aff Unit
shutdown = void <<< take

verifyLogon :: AVar Accounts -> UserName -> Password -> Aff (Maybe Account)
verifyLogon accountsAVar userName password = do
  passwordHash' <- passwordHashHex userName password
  accounts <- AVar.read accountsAVar
  let account' = Map.lookup userName accounts
  pure $ case account' of
    Just (Account { passwordHash }) | passwordHash == passwordHash' -> account'
    _ -> Nothing

createAccount :: AVar Accounts -> Account -> Aff (Either CreateAccountError Unit)
createAccount accountsAVar account@(Account { userName }) = do
  withAVar accountsAVar \accounts -> pure $
    if Map.member userName accounts then
      Tuple accounts (Left AccountAlreadyExists)
    else
      Tuple (Map.insert userName account accounts) (Right unit)

findAccount :: AVar Accounts -> UserName -> Aff (Maybe Account)
findAccount accountsAVar userName = AVar.read accountsAVar >>= pure <<< Map.lookup userName

getAccounts :: AVar Accounts -> Aff (Array Account)
getAccounts accountsAVar = AVar.read accountsAVar >>= values >>> fromFoldable >>> pure

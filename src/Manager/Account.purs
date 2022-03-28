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
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Entity.Account (Account(..))
import Utils (withAVar)

type Accounts = Map String Account

data CreateAccountError = CreateAccountAlreadyExists

startup :: Array Account -> Aff (AVar Accounts)
startup accounts = (accounts <#> \account@(Account { userName }) -> Tuple userName account) # Map.fromFoldable # AVar.new

shutdown :: AVar Accounts -> Aff Unit
shutdown = void <<< AVar.take

verifyLogon :: AVar Accounts -> String -> String -> Aff (Maybe Account)
verifyLogon accountsAVar userName password = do
  passwordHash' <- passwordHashHex userName password
  accounts <- AVar.read accountsAVar
  let account' = Map.lookup userName accounts
  pure $ case account' of
    Just (Account { passwordHash }) -> if passwordHash == passwordHash' then account' else Nothing
    _ -> Nothing

createAccount :: AVar Accounts -> Account -> Aff (Either CreateAccountError Unit)
createAccount accountsAVar account@(Account { userName }) = do
  withAVar accountsAVar \accounts -> pure $
    if Map.member userName accounts then
      Tuple accounts (Left CreateAccountAlreadyExists)
    else
      Tuple (Map.insert userName account accounts) (Right unit)

findAccount :: AVar Accounts -> String -> Aff (Maybe Account)
findAccount accountsAVar userName = AVar.read accountsAVar >>= pure <<< Map.lookup userName

getAccounts :: AVar Accounts -> Aff (Array Account)
getAccounts accountsAVar = AVar.read accountsAVar >>= values >>> fromFoldable >>> pure

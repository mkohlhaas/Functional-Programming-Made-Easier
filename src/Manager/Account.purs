module Manager.Account where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Entity.Account (Account(..))
import Handler.Account (passwordHashHex)

type Accounts = Map String Account

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

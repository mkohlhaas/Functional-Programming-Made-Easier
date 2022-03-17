module Handler.Api.Logon where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans.Class (lift)
import Data.Api.Logon (LogonRequest(..), LogonResponse(..), LogonResults(..))
import Data.Maybe (Maybe(..))
import Data.UUID (emptyUUID)
import Entity.Account (Account(..))
import Foreign.Generic (decodeJSON, encodeJSON)
import HTTPure as HTTPure
import Handler.Class.ApiHandler (class ApiHandler, Handler)
import Manager.Account (verifyLogon)

data Logon = Logon

instance apiHandlerLogon :: ApiHandler Logon where
  handle request _ = do
    logonReq <- runExcept (decodeJSON request :: _ LogonRequest)
    pure $ handler logonReq

handler :: LogonRequest -> Handler
handler (LogonRequest { userName, password }) = do
  { accountsAVar } <- ask
  verifiedAccount <- lift $ verifyLogon accountsAVar userName password
  HTTPure.ok $ encodeJSON
    $ case verifiedAccount of
        Nothing -> LogonResponse LogonResultsFailure
        Just (Account { temporaryPassword }) -> LogonResponse $ LogonResultsSuccess { authToken: emptyUUID, mustChangePassword: temporaryPassword }

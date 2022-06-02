module Handler.Api.QueryUsers where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader.Trans (ask, lift)
import Data.Api.QueryUsers (QueryUsersFailureReason(..), QueryUsersRequest(..), QueryUsersResponse(..), QueryUsersResults(..))
import Data.Either (either, note)
import Entity.Account (Account(..))
import Entity.Session (Session(..))
import Entity.User (User(..))
import Foreign.Generic (encodeJSON)
import HTTPure as HTTPure
import Handler.Api.Common (handleApi)
import Handler.Class.ApiHandler (class ApiHandler, Handler)
import Manager.Account as AM
import Manager.Session (verifySession)
import Record (delete)
import Type.Proxy (Proxy(..))
import Utils (liftSuccess)

data QueryUsers = QueryUsers

instance ApiHandler QueryUsers where
  handle _ = handleApi handler

handler :: QueryUsersRequest -> Handler
handler (QueryUsersRequest { authToken }) = do
  { accountsAVar, sessionsAVar } <- ask
  result <- lift $ runExceptT do
    Session { userName } <- verifySession sessionsAVar authToken <#> note NotAuthenticated # liftSuccess
    Account { admin } <- AM.findAccount accountsAVar userName <#> note NotAuthorized # liftSuccess
    unless admin $ throwError NotAuthorized
    accounts <- lift $ AM.getAccounts accountsAVar
    let users = accounts <#> \(Account r) -> User $ delete (Proxy :: _ "passwordHash") r
    pure $ QueryUsersResultsSuccess { users }
  let ok = HTTPure.ok <<< encodeJSON <<< QueryUsersResponse
  result # either
    ((\reason -> QueryUsersResultsFailure { reason }) >>> ok)
    ok

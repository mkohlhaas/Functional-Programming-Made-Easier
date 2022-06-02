module Handler.Api.CreateUser where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT, withExceptT)
import Control.Monad.Reader.Trans (ask, lift)
import Crypto (passwordHashHex)
import Data.Api.CreateUser (CreateUserFailureReason(..), CreateUserRequest(..), CreateUserResponse(..), CreateUserResults(..))
import Data.Either (either, note)
import Entity.Account (Account(..))
import Entity.Session (Session(..))
import Foreign.Generic (encodeJSON)
import HTTPure as HTTPure
import Handler.Account as AH
import Handler.Api.Common (handleApi)
import Handler.Class.ApiHandler (class ApiHandler, Handler)
import Manager.Account as AM
import Manager.Session (verifySession)
import Record (delete, insert)
import Type.Proxy (Proxy(..))
import Utils (liftSuccess)

data CreateUser = CreateUser

instance ApiHandler CreateUser where
  handle _ = handleApi handler

handler :: CreateUserRequest -> Handler
handler (CreateUserRequest { authToken, user: user' }) = do
  { accountsAVar, sessionsAVar } <- ask
  result <- lift $ runExceptT do -- result :: Either CreateUserFailureReason Unit
    Session { userName } <- verifySession sessionsAVar authToken <#> note NotAuthenticated # liftSuccess -- RHS :: ExceptT CreateUserFailureReason Aff Session
    Account { admin } <- AM.findAccount accountsAVar userName <#> note NotAuthorized # liftSuccess
    unless admin $ throwError NotAuthorized
    passwordHash <- lift $ passwordHashHex user'.userName user'.password
    let
      user = delete (Proxy :: _ "password") user'
      account = Account $ insert (Proxy :: _ "passwordHash") passwordHash user
    AM.createAccount accountsAVar account # liftSuccess # (withExceptT $ const AlreadyExists)
    AH.createAccount account # liftSuccess # withExceptT \(AH.CreateAccountFileError err) -> FileIOError err
  HTTPure.ok $ encodeJSON $ CreateUserResponse $ result # either
    (\reason -> CreateUserResultsFailure { reason })
    (const CreateUserResultsSuccess)

module Main where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.DateTime (diff)
import Data.Either (Either(..), hush)
import Control.Alt (class Alt, (<|>))
import Data.Foldable (class Foldable, foldl)
import Data.JSDate (fromDateTime, toUTCString)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Posix.Signal (Signal(..))
import Data.Time.Duration (Milliseconds)
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (nowDateTime)
import HTTPure (toString)
import HTTPure as HTTPure
import HTTPure.Request (Request)
import HTTPure.Response (ResponseM)
import Handler.Account as AccountHandler
import Handler.Api.Logoff (Logoff)
import Handler.Api.Logon (Logon)
import Handler.Class.ApiHandler (HandlerEnv, handle)
import Manager.Account as AccountManager
import Manager.Session as SessionManager
import Node.Process (onSignal)
import Record (delete)
import Type.Proxy (Proxy(..))

oneOf :: ∀ a t f. Alt t => Foldable f => NonEmpty f (t a) -> t a
oneOf (x :| xs) = foldl (<|>) x xs

loggingRouter :: HandlerEnv -> Request -> ResponseM
loggingRouter env req = do
  id <- liftEffect genUUID
  let
    idStr = " SessionID: " <> show id <> " "
    ts date = toUTCString $ fromDateTime date
  startDate <- liftEffect nowDateTime
  log $ "REQUEST: " <> ts startDate <> idStr <> (show $ delete (Proxy :: _ "body") req)
  res <- router env req
  endDate <- liftEffect nowDateTime
  let duration = " [Duration: " <> show (diff endDate startDate :: Milliseconds) <> " ms]"
  log $ "RESPONSE: " <> ts endDate <> idStr <> (show $ delete (Proxy :: _ "writeBody") res) <> duration
  pure res

router :: HandlerEnv -> Request -> ResponseM
router env { body, method }
  | method == HTTPure.Post = do
      body' <- toString body
      let
        handlers =
          handle (Proxy :: _ Logon) :|
            [ handle (Proxy :: _ Logoff)
            ] <#> (_ $ body')
      case hush $ oneOf handlers of
        Nothing -> HTTPure.badRequest body'
        Just handler -> runReaderT handler env
  | otherwise = HTTPure.methodNotAllowed

port :: Int
port = 3000

main :: Effect Unit
main = launchAff_ do
  loadResults <- AccountHandler.loadAccounts
  case loadResults of
    Left err -> log $ "Cannot load accounts: " <> show err
    Right accounts -> do
      accountsAVar <- AccountManager.startup accounts
      sessionsAVar <- SessionManager.startup
      liftEffect $ do
        shutdown <- HTTPure.serve port (loggingRouter { accountsAVar, sessionsAVar }) $ log $ "Server up running on port: " <> show port
        let
          shutdownServer = do
            log "Shutting down server..."
            launchAff_ $ SessionManager.shutdown sessionsAVar
            launchAff_ $ AccountManager.shutdown accountsAVar
            shutdown $ log "Server shutdown."
        onSignal SIGINT shutdownServer
        onSignal SIGTERM shutdownServer

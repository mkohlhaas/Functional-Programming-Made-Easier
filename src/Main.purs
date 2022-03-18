module Main where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Array (head)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (oneOf, (:|))
import Data.Posix.Signal (Signal(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import HTTPure (toString)
import HTTPure as HTTPure
import HTTPure.Request (Request)
import HTTPure.Response (ResponseM)
import Handler.Account as AccountHandler
import Handler.Api.Logon (Logon)
import Handler.Class.ApiHandler (HandlerEnv, handle)
import Manager.Account as AccountManager
import Node.Process (onSignal)
import Type.Proxy (Proxy(..))

router :: HandlerEnv -> Request -> ResponseM
router env { body, method }
  | method == HTTPure.Post = do
      body' <- toString body
      case hush =<< (head $ oneOf $ (handle body' (Proxy :: _ Logon)) :| []) of
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
      liftEffect $ do
        shutdown <- HTTPure.serve port (router { accountsAVar }) $ log $ "Server up running on port: " <> show port
        let
          shutdownServer = do
            log "Shutting down server..."
            launchAff_ $ AccountManager.shutdown accountsAVar
            shutdown $ log "Server shutdown."
        onSignal SIGINT shutdownServer
        onSignal SIGTERM shutdownServer

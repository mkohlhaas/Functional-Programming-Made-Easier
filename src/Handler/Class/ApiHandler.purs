module Handler.Class.ApiHandler where

import HTTPure.Response

import Control.Monad.Reader (ReaderT)
import Data.Either (Either)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Foreign (MultipleErrors)
import Manager.Account (Accounts)
import Manager.Session (Sessions)
import Type.Proxy (Proxy)

type RequestString = String

class ApiHandler :: ∀ k. k -> Constraint
class ApiHandler a where
  handle :: Proxy a -> RequestString -> Either MultipleErrors Handler

type HandlerEnv =
  { accountsAVar :: AVar Accounts
  , sessionsAVar :: AVar Sessions
  }

type Handler = ReaderT HandlerEnv Aff Response

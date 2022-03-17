module Handler.Class.ApiHandler where

import HTTPure.Response

import Control.Monad.Reader (ReaderT)
import Data.Either (Either)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Foreign (MultipleErrors)
import Manager.Account (Accounts)
import Type.Proxy (Proxy)

class ApiHandler :: ∀ k. k -> Constraint
class ApiHandler a where
  handle :: String -> Proxy a -> Either MultipleErrors Handler

type HandlerEnv = { accountsAVar :: AVar Accounts }
type Handler = ReaderT HandlerEnv Aff Response

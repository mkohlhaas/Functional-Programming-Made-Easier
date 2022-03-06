module Handler.Class.ApiHandler where

import Data.Either (Either)
-- import Data.Unit (Unit)
import Foreign (MultipleErrors)
import Type.Proxy (Proxy)
import HTTPure.Response

class ApiHandler :: ∀ k. k -> Constraint
class ApiHandler a where
  handle :: String -> Proxy a -> Either MultipleErrors ResponseM

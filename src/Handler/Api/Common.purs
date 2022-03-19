module Handler.Api.Common where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Foreign (MultipleErrors)
import Foreign.Class (class Decode)
import Foreign.Generic (decodeJSON)
import Handler.Class.ApiHandler (Handler)

handleApi :: ∀ a. Decode a => (a -> Handler) -> String -> Either MultipleErrors Handler
handleApi handler request = do
  logonReq <- runExcept (decodeJSON request :: _ a)
  pure $ handler logonReq

module Handler.Api.Common where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Foreign (MultipleErrors)
import Foreign.Class (class Decode)
import Foreign.Generic (decodeJSON)
import Handler.Class.ApiHandler (Handler)

type RequestString = String
type RequestHandler = RequestString -> Either MultipleErrors Handler

handleApi :: ∀ a. Decode a => (a -> Handler) -> RequestHandler
handleApi handler request = do
  req <- runExcept (decodeJSON request :: _ a)
  pure $ handler req

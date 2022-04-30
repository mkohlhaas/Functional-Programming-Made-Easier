module Handler.Api.Logoff where

import Prelude

import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans.Class (lift)
import Data.Api.Logoff (LogoffRequest(..), LogoffResponse(..), LogoffResult(..))
import Data.Maybe (Maybe(..))
import Foreign.Generic (encodeJSON)
import HTTPure as HTTPure
import Handler.Api.Common (handleApi)
import Handler.Class.ApiHandler (class ApiHandler, Handler)
import Manager.Session (deleteSession, verifySession)

data Logoff = Logoff

instance ApiHandler Logoff where
  handle _ = handleApi handler

handler :: LogoffRequest -> Handler
handler (LogoffRequest { authToken }) = do
  { sessionsAVar } <- ask
  verifiedSession <- lift $ verifySession sessionsAVar authToken
  response <- case verifiedSession of
    Nothing -> pure $ LogoffResponse LogoffResultsFailure
    Just _ -> do
      lift $ deleteSession sessionsAVar authToken
      pure $ LogoffResponse LogoffResultsSuccess
  HTTPure.ok $ encodeJSON response

module Handler.Api.Logon where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Api.Logon (LogonRequest)
import Foreign.Generic (decodeJSON)
import HTTPure as HTTPure
import HTTPure.Response (ResponseM)
import Handler.Class.ApiHandler (class ApiHandler)

data Logon = Logon

instance apiHandlerLogon :: ApiHandler Logon where
  handle request _ = do
    logonReq <- runExcept (decodeJSON request :: _ LogonRequest)
    pure $ handler logonReq

handler :: LogonRequest -> ResponseM
handler _ = do
  HTTPure.notFound

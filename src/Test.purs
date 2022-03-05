module Test where

import Prelude
import Data.UUID (emptyUUID)
import Foreign.Generic (encodeJSON)
import Api.Logon (LogonRequest(..), LogonResponse(..), LogonResults(..))
import Effect (Effect)
import Effect.Class.Console (log)

test :: Effect Unit
test = do
  -- log $ encodeJSON $ LogonRequest { userName: "joeuser", password: "password" }
  -- log $ encodeJSON $ LogonResponse $ LogonResultsSuccess
  --   { authToken: emptyUUID
  --   , mustChangePassword: true
  --   }
     log $ encodeJSON $ LogonResponse LogonResultsFailure

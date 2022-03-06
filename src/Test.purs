module Test where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)

test :: Effect Unit
test = do
  log "Add some tests!"

  -- log $ encodeJSON $ LogonRequest { userName: "joeuser", password: "password" }
  -- log $ encodeJSON $ LogonResponse $ LogonResultsSuccess
  --   { authToken: emptyUUID
  --   , mustChangePassword: true
  --   }
     -- log $ encodeJSON $ LogonResponse LogonResultsFailure

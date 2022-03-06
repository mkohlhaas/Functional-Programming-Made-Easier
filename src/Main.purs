module Main where

import Prelude

import Node.Process (onSignal)
import Data.Posix.Signal (Signal(..))
import Effect (Effect)
import Effect.Class.Console (log)
import HTTPure as HTTPure
-- import Test (test)
-- import HTTPure.Method (Method(..))
import HTTPure.Request (Request)
import HTTPure.Response (ResponseM)

-- import HTTPure.Server (ServerM)

-- postRouter :: Request -> ResponseM
-- postRouter { path }
--   | path !@ 0 == "this" = HTTPure.ok $ fromMaybe "missing path[1]" $ path !! 1
--   | path !@ 0 == "that" = HTTPure.ok $ fromMaybe "missing path[2]" $ path !! 2
--   | otherwise = HTTPure.notFound

-- router :: Request -> ResponseM
-- router { path: [ "goodbye" ] } = HTTPure.ok "goodbye"
-- router request@{ method }
--   | method == Get = HTTPure.methodNotAllowed
--   | method == Post = postRouter request
-- router _ = HTTPure.notFound

router :: Request -> ResponseM
router _ = HTTPure.notFound

port :: Int
port = 3000

-- main :: Effect Unit
-- main = test

main :: Effect Unit
main = do
  shutdown <- HTTPure.serve port router $ log $ "Server up running on port: " <> show port
  let
    shutdownServer = do
      log "Shutting down server..."
      shutdown $ log "Server shutdown."
  onSignal SIGINT shutdownServer
  onSignal SIGTERM shutdownServer

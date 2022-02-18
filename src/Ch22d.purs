module Ch22d where

import Prelude
import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Effect.Exception (error)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, delay, killFiber, launchAff_, forkAff)

logEverySecond :: Aff Unit
logEverySecond = go 0
  where
  go x = do
    log $ show x
    delay (Milliseconds 1000.0)
    go $ x + 1

readAFileAfterTwoSeconds :: Aff Unit
readAFileAfterTwoSeconds = do
  delay (Milliseconds 2000.0)
  result <- try $ readTextFile ASCII "test.txt"
  case result of
    Right text -> log text
    Left err -> log $ show err

kill :: ∀ a. Fiber a -> Aff Unit
kill = killFiber (error "Killing you softly...")

test :: Effect Unit
test = launchAff_ do
  logger <- forkAff logEverySecond
  fileReader <- forkAff readAFileAfterTwoSeconds
  delay (Milliseconds 5000.0)
  kill logger
  kill fileReader

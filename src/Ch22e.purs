module Ch22e where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, delay, killFiber, launchAff_, forkAff, runAff)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

delayMs :: Number -> Aff Unit
delayMs = delay <<< Milliseconds

logEverySecond :: Aff Unit
logEverySecond = go 0
  where
  go x = do
    log $ show x
    delayMs 1000.0
    go $ x + 1

readAFileAfterTwoSeconds :: Aff String
readAFileAfterTwoSeconds = do
  delayMs 2000.0
  result <- try $ readTextFile ASCII "test.txt" -- readTextFile does not report errors
  pure $ case result of
    Right text -> text -- this path will always be chosen irrespective whether file is existent or not
    Left err -> show err

kill :: ∀ a. Fiber a -> Aff Unit
kill = killFiber (error "Killing you softly...")

test :: Effect Unit
test = do
  fileReader <-
    runAff case _ of
      Left err -> log $ show err
      Right result -> log $ "File contents:\n" <> show result
      $ readAFileAfterTwoSeconds
  launchAff_ do
    logger <- forkAff logEverySecond
    delayMs 5000.0
    kill logger
    kill fileReader

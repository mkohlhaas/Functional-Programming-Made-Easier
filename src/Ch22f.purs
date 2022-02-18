module Ch22f where

import Prelude
import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Effect.Exception (error, message)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Canceler(..), delay, killFiber, launchAff_, runAff, cancelWith)

logEverySecond :: Aff Unit
logEverySecond = go 0
  where
  go x = do
    log $ show x
    delay (Milliseconds 1000.0)
    go $ x + 1

readAFileAfterTwoSeconds :: Aff String
readAFileAfterTwoSeconds = do
  delay (Milliseconds 2000.0)
  -- readTextFile does not report errors
  result <- try $ readTextFile ASCII "somefile.txt"
  pure $ case result of
    Right text -> text -- this path will always be chosen irrespective whether file is existent or not
    Left err -> show err

kill :: ∀ a. Fiber a -> Aff Unit
kill = killFiber (error "Killing you softly...")

test :: Effect Unit
test = do
  logger <- runAff (const $ pure unit)
    $ logEverySecond
        # flip cancelWith (Canceler (log <<< message))
  fileReader <-
    runAff case _ of
      Left err -> log $ show err
      Right result -> log $ "File contents: " <> show result
      $ readAFileAfterTwoSeconds
  launchAff_ do
    delay (Milliseconds 5000.0)
    kill logger
    kill fileReader

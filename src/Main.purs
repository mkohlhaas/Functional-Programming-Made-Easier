module Main where

import Prelude

import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

-- 1. Write StateT type definition.

-- 2. Write runStateT.

-- 3. Write Functor instance.
-- Note: m should be a Functor not a Monad to minimize constraints!

-- 4. Write Apply instance.

-- 5. Write Applicative instance.

-- 6. Write Bind instance.

-- 7. Write Monad instance.

-- 8. Write MonadState instance.

-- 9. Write MonadTrans instance.

-- 10. Write MonadAsk instance.

-- 11. Write MonadTell instance.

-- 12. Write monadAskStateT and monadTellStateT in terms of MonadTrans, i.e. with lift.

-- 13. Write MonadThrow instance.

-- 14. Write MonadError instance.

-- This is our Monad stack:
type AppStack e w s a = ExceptT e (WriterT w (StateT s Effect)) a
type AppM = AppStack String String Int Unit

-- 15. Write runApp to run AppM.

-- 16. Factor out the return type, call it StackResult and update runApp.
-- type StackResult = ...

-- AppEffects stores all side-effects of the monad stack.
type AppEffects = { log :: String, state :: Int, result :: Maybe Unit }

-- AppResult contains our side-effect values from running our Monad Stack AppEffects and, optionally, the error if one occurred.
type AppResult = Tuple (Maybe String) AppEffects

-- 17. Write a mapping function "results" that turns StackResult into an AppResult. Change runApp using "results".
-- results :: StackResult -> AppResult

-- 18. Write helper function "logM" that appends a newline after every string in the writer.

-- 19. Write the application monad.
-- For logging use logM.
-- app :: AppM

-- The body of "app" contains the following actions in the specified order:
-- a) Write to the log "Starting App...".
-- b) Get the State.
-- c) Check the State to make sure it is non-zero, otherwise error with "WE CANNOT HAVE 0 STATE!".
-- d) Add 1 to the State.
-- e) Write to the log "Incremented State".
-- f) Return the Pure Computational Value.

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Exercise Chapter 21."
  result1 <- runApp 0 app
  log $ show result1 ------------- (Tuple (Just "WE CANNOT HAVE A ZERO STATE!") { log: "Starting App...\n", result: Nothing, state: 0 })
  result2 <- runApp 99 app
  log $ show result2 ------------- (Tuple Nothing { log: "Starting App...\nIncremented State\n", result: (Just unit), state: 100 })

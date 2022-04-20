module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

-- 1. Write StateT type definition.

-- 2. Write runStateT instance.

-- 3. Write Functor instance.

-- 4. Write Apply instance.

-- 5. Write Applicative instance.

-- 6. Write Bind instance.

-- 7. Write Monad instance.

-- 8. Write MonadState instance.

-- 9. Write MonadAsk instance.

-- 10. Write MonadTell instance.

-- 11. Write MonadTrans instance.

-- 12. Write monadAskStateT and monadTellStateT in terms of MonadTrans.

-- 13. Write MonadThrow instance.

-- 14. Write MonadError instance.

-- We want this to be our Monad stack:

type AppStack e w s a = ExceptT e (WriterT w (StateT s Effect)) a

-- 15. Write type AppM in terms of AppStack.

-- 16. Write runApp.

-- 17. Factor out the return type.
-- type StackResult = ...

-- 18. Write a type alias using a record to store all side-effects of the monad stack.
-- type AppEffects = ...

-- 19. Write type alias AppResult in terms of AppEffects.

-- 20. Write a mapping function "results" that turns StackResult into an AppResult.
-- results :: StackResult -> AppResult

-- 21. Write the application monad.

-- The body of "app" contains the following actions in the specified order:
-- write to the log "Starting App..."
-- get the State
-- check the State to make sure it is non-zero, otherwise error with "WE CANNOT HAVE 0 STATE!"
-- add 1 to the State
-- write to the log "Incremented State"
-- return the Pure Computational Value

-- app :: AppM

-- 22. Write a helper function "log" that appends a newline after every string in the writer.

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Exercise Chapter 21."
  result1 <- runApp 0 app
  Console.log $ show result1 -- (Tuple Nothing { log: "Starting App...\nIncremented State\n", result: (Just unit), state: 1 })
  result2 <- runApp 99 app
  Console.log $ show result2 -- (Tuple (Just "WE CANNOT HAVE 0 STATE") { log: "Starting App...\n", result: Nothing, state: 99 })

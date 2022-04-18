module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

-- 1. Write Reader, Writer and State monads for your own reference.
-- Hint: You might want to comment out this code.

-- 2. Model a type called RWSResult that contains all three monads.

-- 3. Model a type RWS that works similar to the state monad.

-- Note:
-- Reader = read-only; don't care about the value on output, only on input
-- Writer = write-only; don't care about the value on input, only on output
-- State  = read-write

-- 4. Write a runRWS function similar to runstate.

-- 5. Write a functor instance for RWS.

-- 6. Write an apply instance for RWS
-- You are free to take a shortcut.

-- 7. Write an applicative instance for RWS.
-- Make sure the writer is empty on output.

-- 8. Write a bind instance for RWS.
-- Make sure the writer generates the correct log.

-- 9. Write a monad instance for RWS.

-------------------
-- RWS Monad API --
-------------------

-- 10. Write a tell function.
-- tell :: ∀ r w s. w -> RWS r w s Unit

-- 11. Write an ask function.
-- ask :: ∀ r w s. RWS r w s r

-- 12. Write a get function.
-- get :: ∀ r w s. RWS r w s s

-- 13. Write a put function.
-- put :: ∀ r w s. s -> RWS r w s Unit

---------------------------------
-- Data Structures for Testing --
---------------------------------

-- for the Reader
type Config = { debugModeOn :: Boolean }

-- for the State
type Counter = Int

-- test function
rwsTest :: RWS Config (Array String) Counter Unit
rwsTest = do
  tell [ "test the log" ]
  tell [ "test the log2", "test the log3" ]
  config <- ask
  tell [ "the config is " <> show config ]
  counter <- get
  tell [ "old counter is " <> show counter ]
  put $ counter + 1
  newCounter <- get
  tell [ "new counter is " <> show newCounter ]
  pure unit

----------
-- Main --
----------

main :: Effect Unit
main =
  log "Chapter 19 - RWS Monad"
  log $ show $ runRWS rwsTest { r: { debugModeOn: true }, w: mempty, s: 0 }

-- Output:
-- (Tuple unit
-- { r: { debugModeOn: true },
--   s: 1,
--   w: ["test the log",
--       "test the log2",
--       "test the log3",
--       "the config is { debugModeOn: true }",
--       "old counter is 0",
--       "new counter is 1"] })

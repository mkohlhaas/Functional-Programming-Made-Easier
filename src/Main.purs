module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

-- 1. Write Reader, Writer and State data types which are monads for your own reference.
-- you might want to comment out this code.

-- 2. model a type called rwsresult that contains all three monads

-- 3. model a type rws that works similar to the state monad

-- note:
-- reader = read-only; don't care about the value on output, only on input
-- writer = write-only; don't care about the value on input, only on output
-- state  = read-write

-- 4. write a runrws function similar to runstate

-- 5. write a functor instance for rws

-- 6. write an apply instance for rws (you are free to take a shortcut)

-- 7. write an applicative instance for rws (make sure the writer is empty on output)

-- 8. write a bind instance for rws (make sure the writer generates the correct log)

-- 9. write a monad instance for rws

---------------------
-- monad rws's api --
---------------------

-- 10. write a tell function like writer's
-- tell :: ∀ r w s. w -> rws r w s unit

-- 11. write an ask function like reader's
-- ask :: ∀ r w s. rws r w s r

-- 12. write a get function like state's
-- get :: ∀ r w s. rws r w s s

-- 13. write a put function like state's
-- put :: ∀ r w s. s -> RWS r w s Unit

---------------------------------
-- Data Structures for Testing --
---------------------------------

-- for the Reader part
type Config = { debugModeOn :: Boolean }

-- for the State part
type Counter = Int

-- Use this test function
-- rwsTest :: RWS Config (Array String) Counter Unit
-- rwsTest = do
--   tell [ "test the log" ]
--   tell [ "test the log2", "test the log3" ]
--   config <- ask
--   tell [ "the config is " <> show config ]
--   counter <- get
--   tell [ "old counter is " <> show counter ]
--   put $ counter + 1
--   newCounter <- get
--   tell [ "new counter is " <> show newCounter ]
--   pure unit

main :: Effect Unit
main =
  log "Ch. 19 RWS Monad"
  -- log $ show $ runRWS rwsTest { r: { debugModeOn: true }, w: mempty, s: 0 }

  -- (Tuple unit
  -- { r: { debugModeOn: true },
  --   s: 1,
  --   w: ["test the log",
  --       "test the log2",
  --       "test the log3",
  --       "the config is { debugModeOn: true }",
  --       "old counter is 0",
  --       "new counter is 1"] })

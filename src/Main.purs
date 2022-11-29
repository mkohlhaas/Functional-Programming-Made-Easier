module Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

type RWSResult r w s = { r ∷ r, w ∷ w, s ∷ s } ------------------------ contains all needed information
data RWS r w s a = RWS (RWSResult r w s → Tuple a (RWSResult r w s)) -- works similar to the State monad

-- 1. Write Reader, Writer and State monads for your own reference. (They won't be used, though.)

-- 2. Write a runRWS function similar to runstate.

-- runRWS ∷ ∀ r w s a. RWS r w s a → RWSResult r w s → Tuple a (RWSResult r w s)

-- 3. Write a Functor instance for RWS.

-- 4. Write an Apply instance for RWS. (Take shortcut possibly after you'll have written `bind` instance.)

-- 5. Write an Applicative instance for RWS.

-- 6. Write a Bind instance for RWS.

-- 7. Write a Monad instance for RWS.

-------------------
-- RWS Monad API --
-------------------

-- 8. Write a tell function.
-- tell ∷ ∀ r w s. Semigroup w ⇒ w → RWS r w s Unit

-- 9. Write an ask function.
-- ask ∷ ∀ r w s. Monoid w ⇒ RWS r w s r

-- 10. Write a get function.
-- get ∷ ∀ r w s. Monoid w ⇒ RWS r w s s

-- 11. Write a put function.
-- put ∷ ∀ r w s. s → Monoid w ⇒ RWS r w s Unit

---------------------------------
-- Data Structures for Testing --
---------------------------------

-- the Reader
type Config = { debugModeOn ∷ Boolean }

-- the State
type Counter = Int

-- test function
rwsTest ∷ RWS Config (Array String) Counter Unit
rwsTest = do
  tell [ "test the log" ]
  tell [ "test the log2", "test the log3" ]
  config ← ask
  tell [ "the config is " <> show config ]
  counter ← get
  tell [ "old counter is " <> show counter ]
  put $ counter + 1
  newCounter ← get
  tell [ "new counter is " <> show newCounter ]
  pure unit

----------
-- Main --
----------

main ∷ Effect Unit
main = do
  log "Chapter 19 - RWS Monad."
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

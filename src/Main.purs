module Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

-- 1. Write Reader, Writer and State monads for your own reference.
-- Hint: You might want to comment out this code.
newtype Reader r a = Reader (r -> a)
newtype Writer w a = Writer (Tuple a w)
newtype State s a = State (s -> (Tuple a s))

-- 2. Model a type called RWSResult that contains all three monads.
type RWSResult r w s = { r :: r, w :: w, s :: s }

-- 3. Model a type RWS that works similar to the state monad.
data RWS r w s a = RWS (RWSResult r w s -> Tuple a (RWSResult r w s))

-- Note: Important for coding the instances.
-- Reader = read-only; don't care about the value on output, only on input
-- Writer = write-only; don't care about the value on input, only on output
-- State  = read-write

-- 4. Write a runRWS function similar to runstate.
runRWS :: ∀ r w s a. RWS r w s a -> RWSResult r w s -> Tuple a (RWSResult r w s)
runRWS (RWS f) = f

-- 5. Write a functor instance for RWS.
instance Functor (RWS r w s) where
  map f rws = RWS \rws' -> runRWS rws rws' # \(Tuple a res) -> Tuple (f a) res

-- 6. Write an apply instance for RWS
-- You are free to take a shortcut.
instance Monoid w => Apply (RWS r w s) where
  apply = ap

-- 7. Write an applicative instance for RWS.
-- Make sure the writer is empty on output.
instance Monoid w => Applicative (RWS r w s) where
  pure a = RWS \{ r, s } -> Tuple a { r, w: mempty, s }

-- 8. Write a bind instance for RWS.
-- Make sure the writer generates the correct log.
instance Monoid w => Bind (RWS r w s) where
  bind ma amb = RWS \rws -> runRWS ma rws
    # \(Tuple a rws1@{ w: w1 }) -> runRWS (amb a) rws1
        # \(Tuple b rws2@{ w: w2 }) -> Tuple b rws2 { w = w1 <> w2 }

-- 9. Write a monad instance for RWS.
instance Monoid w => Monad (RWS r w s)

-------------------
-- RWS Monad API --
-------------------

-- 10. Write a tell function.
tell :: ∀ r w s. Semigroup w => w -> RWS r w s Unit
tell w = RWS \{ r, s } -> Tuple unit { r, w, s }

-- 11. Write an ask function.
ask :: ∀ r w s. Monoid w => RWS r w s r
ask = RWS \{ r, s } -> Tuple r { r, w: mempty, s }

-- 12. Write a get function.
get :: ∀ r w s. Monoid w => RWS r w s s
get = RWS \{ r, s } -> Tuple s { r, w: mempty, s }

-- 13. Write a put function.
put :: ∀ r w s. s -> Monoid w => RWS r w s Unit
put s = RWS \{ r } -> Tuple unit { r, w: mempty, s }

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

module Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

type RWSResult r w s = { r ∷ r, w ∷ w, s ∷ s } ------------------------ contains all needed information
data RWS r w s a = RWS (RWSResult r w s → Tuple a (RWSResult r w s)) -- works similar to the State monad

-- 1. Write Reader, Writer and State monads for your own reference.
newtype Reader r a = Reader (r → a)
newtype Writer w a = Writer (Tuple a w)
newtype State s a = State (s → Tuple a s)

-- 2. Write a runRWS function similar to runstate.
runRWS ∷ ∀ r w s a. RWS r w s a → RWSResult r w s → Tuple a (RWSResult r w s)
runRWS (RWS f) = f

-- 3. Write a functor instance for RWS.
instance Functor (RWS r w s) where
  map f x = RWS \rws → runRWS x rws # \(Tuple x' rws') → Tuple (f x') rws'

-- 4. Write an apply instance for RWS. (Take shortcut possibly after you'll have written `bind` instance.)
instance Monoid w ⇒ Apply (RWS r w s) where
  apply = ap

-- 5. Write an applicative instance for RWS.
instance Monoid w ⇒ Applicative (RWS r w s) where
  pure x = RWS \{ r, s } → Tuple x { r, w: mempty, s }

-- 6. Write a bind instance for RWS.
instance Monoid w ⇒ Bind (RWS r w s) where
  bind x f = RWS \rws → runRWS x rws #
    \(Tuple x' rws'@{ w: w' }) → runRWS (f x') rws' #
      \(Tuple x'' rws''@{ w: w'' }) → Tuple x'' rws'' { w = w' <> w'' }

-- 7. Write a monad instance for RWS.
instance Monoid w ⇒ Monad (RWS r w s)

-------------------
-- RWS Monad API --
-------------------

-- 8. Write a tell function.
tell ∷ ∀ r w s. Semigroup w ⇒ w → RWS r w s Unit
tell w = RWS \{ r, s } → Tuple unit { r, w, s }

-- 9. Write an ask function.
ask ∷ ∀ r w s. Monoid w ⇒ RWS r w s r
ask = RWS \{ r, s } → Tuple r { r, w: mempty, s }

-- 10. Write a get function.
get ∷ ∀ r w s. Monoid w ⇒ RWS r w s s
get = RWS \{ r, s } → Tuple s { r, w: mempty, s }

-- 11. Write a put function.
put ∷ ∀ r w s. s → Monoid w ⇒ RWS r w s Unit
put s = RWS \{ r } → Tuple unit { r, w: mempty, s }

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

module Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

type RWSResult r w s = { r ∷ r, w ∷ w, s ∷ s } ------------------------ contains a Reader, a Writer and a State
data RWS r w s a = RWS (RWSResult r w s → Tuple a (RWSResult r w s)) -- works similar to the State monad

-- 1. Write Reader, Writer and State monads for your own reference. (They won't be used, though.)

newtype Reader r a = Reader (r → a)
newtype Writer w a = Writer (Tuple a w)
newtype State s a = State (s → Tuple a s)

-- 2. Write a runRWS function similar to runstate.

runRWS ∷ ∀ r w s a. RWS r w s a → RWSResult r w s → Tuple a (RWSResult r w s)
runRWS (RWS f) = f

-- Note: Instances could be written in terms of runRWS but I find it more complicated.

-- 3. Write a Functor instance for RWS.

instance Functor (RWS r w s) where
  map f (RWS g) = RWS $ \rwsRes → g rwsRes # \(Tuple a rwsRes') → Tuple (f a) rwsRes'

-- 4. Write an Apply instance for RWS.

instance Apply (RWS r w s) where
  apply (RWS f) (RWS g) = RWS $ \rwsRes → f rwsRes # \(Tuple f' rwsRes') → g rwsRes' # \(Tuple a rwsRes'') → Tuple (f' a) rwsRes''

-- instance Apply (RWS r w s) where
--   apply = ap

-- 5. Write an Applicative instance for RWS.

instance Applicative (RWS r w s) where
  pure a = RWS $ \rwsRes → Tuple a rwsRes

-- 6. Write a Bind instance for RWS.

instance Bind (RWS r w s) where
  bind (RWS f) g = RWS $ \rwsRes → f rwsRes # (\(Tuple a rwsRes') → g a # (\(RWS h) → h rwsRes'))

-- 7. Write a Monad instance for RWS.

instance Monad (RWS r w s)

-------------------
-- RWS Monad API --
-------------------

-- 8. Write a tell function.

tell ∷ ∀ r w s. Semigroup w ⇒ w → RWS r w s Unit
tell w = RWS $ \rwsRes@{ w: w' } → Tuple unit rwsRes { w = w' <> w }

-- 9. Write an ask function.

ask ∷ ∀ r w s. RWS r w s r
ask = RWS $ \rwsRes@{ r } → Tuple r rwsRes

-- 10. Write a get function.

get ∷ ∀ r w s. RWS r w s s
get = RWS $ \rwsRes@{ s } → Tuple s rwsRes

-- 11. Write a put function.

put ∷ ∀ r w s. s → RWS r w s Unit
put s = RWS $ \rwsRes → Tuple unit rwsRes { s = s }

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
  log $ show $ runRWS rwsTest { r: { debugModeOn: true }, w: mempty, s: 0 } ==
    ( Tuple unit
        { r: { debugModeOn: true }
        , s: 1
        , w: [ "test the log", "test the log2", "test the log3", "the config is { debugModeOn: true }", "old counter is 0", "new counter is 1" ]
        }
    )

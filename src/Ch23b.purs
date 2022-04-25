module Ch23b where

import Prelude

import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State.Trans (StateT, get, modify_, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, forkAff, delay)
import Effect.Aff.Bus (BusRW)
import Effect.Aff.Bus as Bus
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)

----------------
-- Data Types --
----------------
type Config = BusRW String
type State = Int
-- type State = { count :: Int }
type FiberM a = ReaderT Config (StateT State Aff) a

-----------
-- Aff's --
-----------
delayMs :: Number -> Aff Unit
delayMs = delay <<< Milliseconds

randomAff :: Aff Number
randomAff = liftEffect random

constant :: Aff Number
constant = pure 0.4

delayRandom :: Aff Number
delayRandom = delayMs 1000.0 *> randomAff

runFiberM :: BusRW String -> FiberM Unit -> Aff Unit
runFiberM bus = void <<< forkAff <<< flip runStateT 3 <<< flip runReaderT bus

--------------
-- FiberM's --
--------------
liftAffToFiberM :: Aff ~> FiberM
liftAffToFiberM = lift <<< lift

logger :: FiberM Unit
logger = forever do
  bus <- ask
  s <- liftAffToFiberM $ Bus.read bus
  log $ "Logger " <> s

randomGenerator :: Int -> String -> (Number -> Boolean) -> FiberM Unit
randomGenerator id predInfo pred = do
  count <- get
  unless (count <= 0) do
    bus <- ask
    liftAffToFiberM do
      n <- delayRandom
      log $ "Random number " <> show id <> ": " <> show n
      when (pred n) $ flip Bus.write bus $ show id <> ": found a value that is " <> predInfo <> " (" <> show n <> ")."
    modify_ (_ - 1)
    -- modify_ _ { count = count - 1 }
    randomGenerator id predInfo pred

----------
-- Test --
----------
test :: Effect Unit
test = launchAff_ do
  forkFiberM <- runFiberM <$> Bus.make
  forkFiberM logger
  forkFiberM $ randomGenerator 1 "greater than 0.5" (_ > 0.5)
  forkFiberM $ randomGenerator 2 "less than 0.5" (_ < 0.5)
  forkFiberM $ randomGenerator 3 "greater than 0.1" (_ > 0.1)

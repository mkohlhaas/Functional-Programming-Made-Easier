module Ch23a where

import Prelude

import Effect (Effect)
import Effect.Exception (error)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay, launchAff_, forkAff, killFiber, joinFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class.Console (log)

----------------
-- Data Types --
----------------

data TickTock = Tick | Tock
data BombState = WaitingForTick | WaitingForTock
type Count = Int

---------------
-- Instances --
---------------

derive instance eqTickTock :: Eq TickTock

---------------
-- Functions --
---------------

clock :: AVar TickTock -> Aff Unit
clock ttAVar = do
  void $ AVar.take ttAVar
  delay (Milliseconds 1000.0)
  AVar.put Tick ttAVar
  void $ AVar.take ttAVar
  delay (Milliseconds 1000.0)
  AVar.put Tock ttAVar
  clock ttAVar

bomb :: AVar TickTock -> Count -> Aff Unit
bomb ttAVar detonationCount = go 0 WaitingForTick
  where
  go :: Int -> BombState -> Aff Unit
  go count state =
    if count == detonationCount then do log "BOOM!!"
    else do
      delay (Milliseconds 100.0)
      tt <- AVar.read ttAVar
      case state, tt of
        WaitingForTick, Tick -> log "Tick" *> go (count + 0) WaitingForTock
        WaitingForTock, Tock -> log "Tock" *> go (count + 1) WaitingForTick
        _, _ -> go count state

----------
-- Test --
----------

test :: Effect Unit
test =
  let
    detonationCount = 3
  in
    launchAff_ do
      ttAVar <- AVar.empty
      clockFiber <- forkAff $ clock ttAVar
      bombFiber <- forkAff $ bomb ttAVar detonationCount
      AVar.put Tick ttAVar
      joinFiber bombFiber
      killFiber (error "Exploded") clockFiber

module Ch23a where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_, forkAff, killFiber, joinFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class.Console (log)
import Effect.Exception (error)

----------------
-- Data Types --
----------------
data TickTock = Tick | Tock
data BombState = AwaitingTick | AwaitingTock
type Count = Int
type Delay = Number

---------------
-- Instances --
---------------
derive instance Generic TickTock _
instance Show TickTock where
  show = genericShow

---------------
-- Functions --
---------------
delayMs :: Number -> Aff Unit
delayMs = delay <<< Milliseconds

tickTockClock :: AVar TickTock -> TickTock -> Delay -> Aff Unit
tickTockClock ttAVar ticktock delay = do
  void $ AVar.take ttAVar
  delayMs delay
  AVar.put ticktock ttAVar
  log $ "Clock: " <> show ticktock

clock :: AVar TickTock -> Aff Unit
clock ttAVar = do
  tickTockClock ttAVar Tick 1000.0
  tickTockClock ttAVar Tock 1000.0
  clock ttAVar

bomb :: AVar TickTock -> Count -> Aff Unit
bomb ttAVar detonationCount = go 0 AwaitingTick
  where
  go :: Int -> BombState -> Aff Unit
  go count state =
    if count == detonationCount then log "BOOM!!"
    else do
      delayMs 100.0
      tt <- AVar.read ttAVar
      case state, tt of
        AwaitingTick, Tick -> log "Bomb: Tick" *> go (count + 0) AwaitingTock
        AwaitingTock, Tock -> log "Bomb: Tock" *> go (count + 1) AwaitingTick
        _, _ -> log "Looping ..." *> go count state

----------
-- Test --
----------
test :: Effect Unit
test = launchAff_ do
  ttAVar <- AVar.empty
  clockFiber <- forkAff $ clock ttAVar
  bombFiber <- forkAff $ bomb ttAVar detonationCount
  AVar.put Tick ttAVar
  joinFiber bombFiber
  killFiber (error "Exploded") clockFiber
  where
  detonationCount = 3

module Main where

import Prelude
import Data.Int (even, odd)
import Effect (Effect)
import Effect.Console (log)

------------------
-- Type Classes --
------------------

---------------
-- Predicate --
---------------

-- What is the polarity of the polymorphic parameter a ?
-- What kind of functor do we need ?
data Predicate a = Predicate (a -> Boolean)

-------------------
-- Moore Machine --
-------------------

-- What are the polarities of the polymorphic parameters a and b ?
-- What kind of functor do we need ?
data Moore s a b = Moore s (s -> b) (s -> a -> s)

data OvenState = Off | Bake | Idling
data Heat = HeatOn | HeatOff
data InputSignal = BakePressed | OffPressed | TooHot | TooCold

outputFn :: OvenState -> Heat
outputFn Off = HeatOff
outputFn Bake = HeatOn
outputFn Idling = HeatOff

transitionFn :: OvenState -> InputSignal -> OvenState
transitionFn Off BakePressed = Bake
transitionFn Bake OffPressed = Off
transitionFn Bake TooHot = Idling
transitionFn Idling TooCold = Bake
transitionFn Idling OffPressed = Off
transitionFn s _ = s

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Chapter 15."
  -- log $ show $ runPredicate   (Predicate even) $ 10               -- true
  -- log $ show $ runPredicate   (Predicate even) $ 11               -- false
  -- log $ show $ runPredicate   (Predicate odd)  $ 10               -- false
  -- log $ show $ runPredicate   (Predicate odd)  $ 11               -- true
  -- log $ show $ runPredicate   (cmap (_ + 1) (Predicate odd)) 10   -- true
  -- log $ show $ runPredicate   (cmap (_ + 2) (Predicate odd)) 10   -- false
  -- log $ show $ runPredicate   ((_ + 1) >$<  (Predicate odd)) 10   -- true
  -- log $ show $ runPredicate   ((_ + 2) >$<  (Predicate odd)) 10   -- false
  -- log $ show $ runFoldL addr  [1, 2, 3]                           -- 6               addr leverages Moore
  -- log $ show $ runFoldL addr  (1.0 : 2.0 : 3.0 : Nil)             -- 6.0
  -- log $ show $ runFoldL sizer [ "This", "is", "the", "test" ]     -- "Size is 13"    sizer leverages addr

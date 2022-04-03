module Main where

import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, (==), (<=), (<), (>), (>=), ($))

-------------------------------------------------------------
-- Define data types Maybe and Either. Do not import them! --
-------------------------------------------------------------

----------------
-- Data Types --
----------------

---------------------
-- Class Instances --
---------------------

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Exercise Chapter 7."
  log "Derive missing instances! Only necessary imports are allowed."
  log $ show $ Just 5 == Just 5     -- true
  log $ show $ Just 5 == Just 2     -- false
  log $ show $ Just 5 == Nothing    -- false
  log $ show $ Nothing == Just 5    -- false
  log $ show $ Nothing == Nothing   -- true
  log $ show $ Left "left"          -- (Left "left")
  log $ show $ Right (Just 42)      -- (Right (Just 42))
  log $ show $ Left 1 < Left 2      -- true
  log $ show $ Left 2 < Left 1      -- false
  log $ show $ Just 1 < Just 5      -- true
  log $ show $ Just 5 <= Just 5     -- true
  log $ show $ Just 5 > Just 10     -- false
  log $ show $ Just 10 >= Just 10   -- true
  log $ show $ Just 99 > Nothing    -- true
  log $ show $ Just 99 < Nothing    -- false
  log $ show $ Just "abc"           -- (Just "abc")
  log $ show $ Nothing              -- Nothing

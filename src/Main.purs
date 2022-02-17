module Main where

import Prelude (Unit, discard, (<>))

import Effect (Effect)
import Effect.Console (log)

-------------------
-- JS Primitives --
-------------------

foreign import ordIntImpl :: Ordering -> Ordering -> Ordering -> Int -> Int -> Ordering
foreign import eqIntImpl :: Int -> Int -> Boolean
foreign import showIntImpl :: Int -> String
foreign import showStringImpl :: String -> String

------------------------------------
-- 1. Change Data Type 'Ordering' --
------------------------------------

data Ordering = DONT_KNOW

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Chapter 7."
  log "Implement ALL missing functions by hand! "
  log "No further imports necessary!"
  -- log $ show $ Just 5  == Just 5    -- true
  -- log $ show $ Just 5  == Just 2    -- false
  -- log $ show $ Just 5  == Nothing   -- false
  -- log $ show $ Nothing == Just 5    -- false
  -- log $ show $ Nothing == Nothing   -- true
  -- log $ show $ Left "left"          -- (Left "left")
  -- log $ show $ Right (Just 42)      -- (Right (Just 42))
  -- log $ show $ Just 1  <  Just 5    -- true
  -- log $ show $ Just 5  <= Just 5    -- true
  -- log $ show $ Just 5  >  Just 10   -- false
  -- log $ show $ Just 10 >= Just 10   -- true
  -- log $ show $ Just 99 >  Nothing   -- true
  -- log $ show $ Just 99 <  Nothing   -- false
  -- log $ show $ Just "abc"           -- (Just "abc")
  -- log $ show $ Nothing              -- Nothing

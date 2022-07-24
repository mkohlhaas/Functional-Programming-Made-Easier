module Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

-- Define data types Maybe and Either - no imports!
-- Implement instances handishly - no derivations, no shortcuts, unless absolutely necessary!

----------
-- Main --
----------

main ∷ Effect Unit
main = do
  log "Exercise Chapter 19."
  log "-----------------"
  log "-- Maybe Monad --"
  log "-----------------"
  log $ show $ Just (_ * 10) <*> Just 20 ------------------------------------- (Just 200)
  log $ show $ Just (_ * 10) <*> pure 20 ------------------------------------- (Just 200)
  log $ show $ Just 20 >>= pure <<< (_ * 10) --------------------------------- (Just 200)
  log $ show do -------------------------------------------------------------- (Just 200)
    x ← Just 20
    let y = x * 10
    pure y
  log $ show $ Just 20 >>= const Nothing >>= \y → Just $ y + 42 -------------- Nothing
  log $ show do -------------------------------------------------------------- Nothing
    _ ← Just 20
    y ← Nothing
    pure $ y + 42
  log "------------------"
  log "-- Either Monad --"
  log "------------------"
  log $ show $ Right (_ * 10) <*> (Right 20 ∷ Either Unit _) ----------------- (Right 200)
  log $ show $ Right (_ * 10) <*> (pure 20 ∷ Either Unit _) ------------------ (Right 200)
  log $ show $ (Right 20 ∷ Either Unit _) >>= pure <<< (_ * 10) -------------- (Right 200)
  log $ show do -------------------------------------------------------------- (Right 200)
    x ← Right 20 ∷ Either Unit _
    let y = x * 10
    pure y
  log $ show $ Right 20 >>= const (Left "error") >>= \y → Right $ y + 42 ----- (Left "error")
  log $ show do -------------------------------------------------------------- (Left "error")
    _ ← Right 20
    y ← Left "error"
    pure $ y + 42

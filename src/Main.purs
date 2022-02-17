module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

-----------
-- Maybe --
-----------

-- 1. Define data type Maybe
-- 2. Create Show instance
-- 3. Create Functor instance
-- 4. Create Apply instance
-- 5. Create Applicative instance
-- 6. Create Bind instance
-- 7. Create Monad instance

------------
-- Either --
------------

-- 1. Define data type Either
-- 2. Create Show instance
-- 3. Derive Functor instance
-- 4. Create Apply instance
-- 5. Create Applicative instance
-- 6. Create Bind instance
-- 7. Create Monad instance

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Ch. 19."
  -- log "-- Maybe Monad --"
  -- log $ show $ Just (_ * 10) <*> Just 20
  -- log $ show $ Just (_ * 10) <*> pure 20
  -- log $ show $ Just 20 >>= pure <<< (_ * 10)
  -- log $ show do
  --    x <- Just 20
  --    let y = x * 10
  --    pure y
  -- log $ show $ Just 20 >>= const Nothing >>= \y -> Just $ y + 42
  -- log $ show do
  --    _ <- Just 20
  --    y <- Nothing
  --    pure $ y + 42
  -- log "-- Either Monad --"
-- log $ show $ Right (_ * 10) <*> (Right 20 :: Either Unit _)
-- log $ show $ Right (_ * 10) <*> (pure 20 :: Either Unit _)
-- log $ show $ (Right 20 :: Either Unit _) >>= pure <<< (_ * 10)
-- log $ show do
--   x <- Right 20 :: Either Unit _
--   let y = x * 10
--   pure y
-- log $ show $ Right 20 >>= const (Left "error") >>= \y -> Right $ y + 42
-- log $ show do
--   _ <- Right 20
--   y <- Left "error"
--   pure $ y + 42

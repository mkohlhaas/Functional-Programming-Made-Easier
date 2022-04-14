module Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

-----------
-- Maybe --
-----------

-- 1. Define data type Maybe
data Maybe a = Nothing | Just a

-- 2. Create Show instance
derive instance Generic (Maybe a) _

instance Show a => Show (Maybe a) where
  show = genericShow

-- 3. Create Functor instance
instance Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

-- 4. Create Apply instance
instance Apply Maybe where
  apply (Just f) (Just x) = Just $ f x
  apply _ _ = Nothing

-- 5. Create Applicative instance
instance Applicative Maybe where
  pure = Just

-- 6. Create Bind instance
instance Bind Maybe where
  bind Nothing _ = Nothing
  bind (Just x) f = f x

-- 7. Create Monad instance
instance Monad Maybe

------------
-- Either --
------------

-- 1. Define data type Either
data Either a b = Left a | Right b

-- 2. Create Show instance
derive instance Generic (Either a b) _
instance (Show a, Show b) => Show (Either a b) where
  show = genericShow

-- 3. Derive Functor instance
instance Functor (Either a) where
  map _ (Left x) = Left x
  map f (Right y) = Right $ f y

-- 4. Create Apply instance
instance Apply (Either a) where
  apply (Left x) _ = Left x
  apply (Right f) y = f <$> y

-- 5. Create Applicative instance
instance Applicative (Either a) where
  pure = Right

-- 6. Create Bind instance
instance Bind (Either a) where
  bind (Left x) _ = Left x
  bind (Right y) f = f y

-- 7. Create Monad instance
instance Monad (Either a)

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Exercise Chapter 19."
  log "-----------------"
  log "-- Maybe Monad --"
  log "-----------------"
  log $ show $ Just (_ * 10) <*> Just 20                                    -- (Just 200)
  log $ show $ Just (_ * 10) <*> pure 20                                    -- (Just 200)
  log $ show $ Just 20 >>= pure <<< (_ * 10)                                -- (Just 200)
  log $ show do                                                             -- (Just 200)
    x <- Just 20
    let y = x * 10
    pure y
  log $ show $ Just 20 >>= const Nothing >>= \y -> Just $ y + 42            -- Nothing
  log $ show do                                                             -- Nothing
    _ <- Just 20
    y <- Nothing
    pure $ y + 42
  log "------------------"
  log "-- Either Monad --"
  log "------------------"
  log $ show $ Right (_ * 10) <*> (Right 20 :: Either Unit _)               -- (Right 200)
  log $ show $ Right (_ * 10) <*> (pure 20 :: Either Unit _)                -- (Right 200)
  log $ show $ (Right 20 :: Either Unit _) >>= pure <<< (_ * 10)            -- (Right 200)
  log $ show do                                                             -- (Right 200)
    x <- Right 20 :: Either Unit _
    let y = x * 10
    pure y
  log $ show $ Right 20 >>= const (Left "error") >>= \y -> Right $ y + 42   -- (Left "error")
  log $ show do                                                             -- (Left "error")
    _ <- Right 20
    y <- Left "error"
    pure $ y + 42

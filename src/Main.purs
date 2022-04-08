module Main where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Common (toUpper)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, class Eq, class Functor, class Show, discard, flip, identity, map, show, ($), (<$>), (/), (<>), (==), (*), (<<<))

-------------------------------------
-- Define Type Class for Bifunctor --
-------------------------------------
class Bifunctor f where
  bimap :: ∀ a b c d. (a -> c) -> (b -> d) -> f a b -> f c d

rmap :: ∀ f a b c. Bifunctor f => (b -> c) -> f a b -> f a c
rmap = bimap identity

lmap :: ∀ f a b c. Bifunctor f => (a -> c) -> f a b -> f c b
lmap = flip bimap identity

------------------------------------------------------------
-- Define Types for Maybe, Either and Tuple - No imports! --
------------------------------------------------------------

-----------
-- Maybe --
-----------
data Maybe a = Nothing | Just a

derive instance Eq a => Eq (Maybe a)

derive instance Generic (Maybe a) _

instance Show a => Show (Maybe a) where
  show = genericShow

derive instance Functor Maybe

------------
-- Either --
------------
data Either a b = Left a | Right b

derive instance Generic (Either a b) _

instance (Show a, Show b) => Show (Either a b) where
  show = genericShow

derive instance Functor (Either a)

instance Bifunctor Either where
  bimap f _ (Left a) = Left $ f a
  bimap _ f (Right b) = Right $ f b

-----------
-- Tuple --
-----------
data Tuple a b = Tuple a b

derive instance (Eq a, Eq b) => Eq (Tuple a b)

derive instance Generic (Tuple a b) _

instance (Show a, Show b) => Show (Tuple a b) where
  show = genericShow

derive instance Functor (Tuple a)

instance Bifunctor Tuple where
  bimap f g (Tuple a b) = Tuple (f a) (g b)

--------------
-- Threeple --
--------------
data Threeple a b c = Threeple a b c

derive instance Generic (Threeple a b c) _

instance (Show a, Show b, Show c) => Show (Threeple a b c) where
  show = genericShow

derive instance Functor (Threeple a b)

instance Bifunctor (Threeple a) where
  bimap f g (Threeple a b c) = Threeple a (f b) (g c)

----------
-- Main --
----------
main :: Effect Unit
main = do
  log "Exercise Chapter 13."
  log $ show $ (_ / 2) <$> Just 10                                                                                 -- (Just 5)
  log $ show $ (_ / 2) <$> Nothing                                                                                 -- Nothing
  log $ show $ (_ / 2) <$> (Right 10 :: Either Unit _)                                                             -- (Right 5)
  log $ show $ (_ / 2) <$> Left "error reason"                                                                     -- (Left "error reason")
  log $ show $ (_ / 2) <$> Tuple 10 20                                                                             -- (Tuple 10 10)
  log $ show $ (_ / 2) <$> Threeple 10 20 40                                                                       -- (Threeple 10 20 20)
  log $ show $ "Maybe Identity for Nothing: " <> show ((identity <$> Nothing) == (Nothing :: Maybe Unit))          -- Maybe Identity for Nothing: true
  log $ show $ "Maybe Identity for Just: " <> show ((identity <$> Just [1, 2]) == Just [1, 2])                     -- Maybe Identity for Just: true
  let g x = x * 2
      f x = x * 3
  log $ show $ "Maybe Composition for Nothing: " <> show ((map (g <<< f) Nothing) == (map f <<< map g) Nothing)    -- "Maybe Composition for Nothing: true"
  log $ show $ "Maybe Composition for Just: " <> show ((map (g <<< f) (Just 60)) == (map f <<< map g) (Just 60))   -- "Maybe Composition for Just: true"
  log $ show $ "Tuple Identity: " <> show ((identity <$> Tuple 10 20) == Tuple 10 20)                              -- "Tuple Identity: true"
  log $ show $ "Tuple Composition : " <> show ((map (g <<< f) (Tuple 10 20)) == (map f <<< map g) (Tuple 10 20))   -- "Tuple Composition : true"
  log $ show $ rmap  (_ * 2) $ Left "error reason"                                                                 -- (Left "error reason")
  log $ show $ lmap  toUpper $ (Left "error reason" :: Either _ Unit)                                              -- (Left "ERROR REASON")
  log $ show $ rmap  (_ * 2) $ (Right 10 :: Either Unit Int)                                                       -- (Right 20)
  log $ show $ lmap  toUpper $ Right 10                                                                            -- (Right 10)
  log $ show $ rmap  (_ * 2) $ Tuple 80 40                                                                         -- (Tuple 80 80)
  log $ show $ lmap  (_ / 2) $ Tuple 80 40                                                                         -- (Tuple 40 40)
  log $ show $ bimap (_ / 2) (_ * 2) $ Tuple 80 40                                                                 -- (Tuple 40 80)
  log $ show $ rmap  (_ * 2) $ Threeple 99 80 40                                                                   -- (Threeple 99 80 80)
  log $ show $ lmap  (_ / 2) $ Threeple 99 80 40                                                                   -- (Threeple 99 40 40)
  log $ show $ bimap (_ / 2) (_ * 2) $ Threeple 99 80 40                                                           -- (Threeple 99 40 80)

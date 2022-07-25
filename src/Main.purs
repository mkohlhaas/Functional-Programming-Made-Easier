module Main where

import Data.String.Common (toUpper)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, class Eq, class Functor, class Show, discard, flip, identity, map, show, ($), (<$>), (/), (<>), (==), (*), (<<<))

-------------------------------------
-- Define Type Class for Bifunctor --
-------------------------------------

------------------------------------------------------------
-- Define Types for Maybe, Either and Tuple - No imports! --
------------------------------------------------------------

--------------
-- Threeple --
--------------
data Threeple a b c = Threeple a b c

----------
-- Main --
----------
main ∷ Effect Unit
main = do
  log "Exercise Chapter 13."
  log $ show $ (_ / 2) <$> Just 10 --------------------------------------------------------------------------------- (Just 5)
  log $ show $ (_ / 2) <$> Nothing --------------------------------------------------------------------------------- Nothing
  log $ show $ (_ / 2) <$> (Right 10 ∷ Either Unit _) -------------------------------------------------------------- (Right 5)
  log $ show $ (_ / 2) <$> Left "error reason" --------------------------------------------------------------------- (Left "error reason")
  log $ show $ (_ / 2) <$> Tuple 10 20 ----------------------------------------------------------------------------- (Tuple 10 10)
  log $ show $ (_ / 2) <$> Threeple 10 20 40 ----------------------------------------------------------------------- (Threeple 10 20 20)
  log $ show $ "Maybe Identity for Nothing: " <> show ((identity <$> Nothing) == (Nothing ∷ Maybe Unit)) ----------- Maybe Identity for Nothing: true
  log $ show $ "Maybe Identity for Just: " <> show ((identity <$> Just [ 1, 2 ]) == Just [ 1, 2 ]) ----------------- Maybe Identity for Just: true
  let
    g x = x * 2
    f x = x * 3
  log $ show $ "Maybe Composition for Nothing: " <> show ((map (f <<< g) Nothing) == (map f <<< map g) Nothing) ---- "Maybe Composition for Nothing: true"
  log $ show $ "Maybe Composition for Just: " <> show ((map (f <<< g) (Just 60)) == (map f <<< map g) (Just 60)) --- "Maybe Composition for Just: true"
  log $ show $ "Tuple Identity: " <> show ((identity <$> Tuple 10 20) == Tuple 10 20) ------------------------------ "Tuple Identity: true"
  log $ show $ "Tuple Composition : " <> show ((map (f <<< g) (Tuple 10 20)) == (map f <<< map g) (Tuple 10 20)) --- "Tuple Composition : true"
  log $ show $ rmap (_ * 2) $ Left "error reason" ------------------------------------------------------------------ (Left "error reason")
  log $ show $ lmap toUpper $ (Left "error reason" ∷ Either String Unit) ------------------------------------------- (Left "ERROR REASON")
  log $ show $ rmap (_ * 2) $ (Right 10 ∷ Either Unit Int) --------------------------------------------------------- (Right 20)
  log $ show $ lmap toUpper $ Right 10 ----------------------------------------------------------------------------- (Right 10)
  log $ show $ rmap (_ * 2) $ Tuple 80 40 -------------------------------------------------------------------------- (Tuple 80 80)
  log $ show $ lmap (_ / 2) $ Tuple 80 40 -------------------------------------------------------------------------- (Tuple 40 40)
  log $ show $ bimap (_ / 2) (_ * 2) $ Tuple 80 40 ----------------------------------------------------------------- (Tuple 40 80)
  log $ show $ rmap (_ * 2) $ Threeple 99 80 40 -------------------------------------------------------------------- (Threeple 99 80 80)
  log $ show $ lmap (_ / 2) $ Threeple 99 80 40 -------------------------------------------------------------------- (Threeple 99 40 40)
  log $ show $ bimap (_ / 2) (_ * 2) $ Threeple 99 80 40 ----------------------------------------------------------- (Threeple 99 40 80)

module Main where

import Prelude

import Data.Either (Either(..), hush, note)
import Data.Functor.Invariant (class Invariant, imap)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

positive :: Maybe Int -> Maybe Int
positive x = if x >= Just 0 then x else Nothing

positiveEither' :: Either Unit Int -> Either Unit Int
positiveEither' (Right x) = positive (Just x) # note unit
positiveEither' (Left _) = positive Nothing # note unit

data Iso a b = Iso (a -> b) (b -> a)

isoMaybeEither :: Iso (Maybe Int) (Either Unit Int)
isoMaybeEither = Iso (note unit) hush

newtype Endo a = Endo (a -> a)

instance Invariant Endo where
  imap ab ba (Endo aa) = Endo (ab <<< aa <<< ba)

iso :: ∀ a b f. Invariant f => Iso a b -> f a -> f b
iso (Iso to from) = imap to from

positiveEither :: Either Unit Int -> Either Unit Int
positiveEither = f
  where
  (Endo f) = iso isoMaybeEither (Endo positive)

main :: Effect Unit
main = do
  log $ show $ positive (Just 5)                                                    -- (Just 5)
  log $ show $ positive (Just (-1))                                                 -- Nothing
  log $ show $ positiveEither $ Right 5                                             -- (Right 5)
  log $ show $ positiveEither $ Left unit                                           -- (Left unit)
  log $ show $ positive (Just 5) # note unit                                        -- (Right 5)
  log $ show $ positive (Just (-1)) # note unit                                     -- (Left unit)
  log $ show $ positiveEither' $ Right 5                                            -- (Right 5)
  log $ show $ positiveEither' $ Left unit                                          -- (Left unit)

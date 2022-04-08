module Main where

import Prelude

import Data.Either (Either(..), hush, note)
import Data.Functor.Invariant (class Invariant, imap)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

-- data Iso a b = Iso (a -> b) (b -> a)

-- iso :: ∀ a b f. Invariant f => Iso a b -> f a -> f b
-- iso (Iso to from) = imap to from

-- isoMaybeEither :: String -> Iso (Maybe Int) (Either String Int)
-- isoMaybeEither err = Iso (note err) hush

newtype Endo a = Endo (a -> a)

instance Invariant Endo where
  imap ab ba (Endo aa) = Endo (ab <<< aa <<< ba)

positive :: Maybe Int -> Maybe Int
positive x = if x >= Just 0 then x else Nothing

positiveEither :: Either String Int -> Either String Int
positiveEither = f
  where
  (Endo f) = imap (note "Not a positive integer") hush (Endo positive)

positiveEither' :: Either String Int -> Either String Int
positiveEither' (Right x) = positive (Just x) # note "Not a positive integer"
positiveEither' (Left _) = positive Nothing # note "Not a positive integer"

main :: Effect Unit
main = do
  log $ show $ positive (Just 5)                                      -- (Just 5)
  log $ show $ positive (Just (-1))                                   -- Nothing
  log $ show $ positiveEither $ Right 5                               -- (Right 5)
  log $ show $ positiveEither $ Left "Error!"                         -- (Left "Not a positive integer")
  log $ show $ positive (Just 5) # note "Not a positive integer"      -- (Right 5)
  log $ show $ positive (Just (-1)) # note "Not a positive integer"   -- (Left "Not a positive integer")
  log $ show $ positiveEither' $ Right 5                              -- (Right 5)
  log $ show $ positiveEither' $ Left "Error!"                        -- (Left "Not a positive integer")

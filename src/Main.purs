module Main where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldr, foldrDefault, intercalate)
import Data.Int (even)
import Data.Maybe (Maybe(..))
import Data.Traversable (class Traversable, traverse, sequence)
import Effect (Effect)
import Effect.Console (log)

-------------------------
-- Applicative Example --
-------------------------

fullName :: String -> String -> String -> String
fullName first middle last = first <> " " <> middle <> " " <> last

errIfMissing :: Maybe String -> String -> Either String String
errIfMissing Nothing err = Left err
errIfMissing (Just s) _ = Right s

fullNameEither :: Maybe String -> Maybe String -> Maybe String -> Either String String
fullNameEither first middle last =
  fullName
    <$> (first `errIfMissing` "First name missing.")
    <*> (middle `errIfMissing` "Middle name missing.")
    <*> (last `errIfMissing` "Last name missing.")

-------------------------
-- Traversable Example --
-------------------------

data Liste a = Nil | Cons a (Liste a)

infixr 6 Cons as :

instance showList :: Show a => Show (Liste a) where
  show Nil = "Nil"
  show xs = "[" <> intercalate " : " (show <$> xs) <> " : Nil]"

instance Functor Liste where
  map _ Nil = Nil
  map f (a : as) = f a : map f as

instance Foldable Liste where
  foldl f = foldlDefault f
  foldr f = foldrDefault f
  foldMap _ Nil = mempty
  foldMap f (a : as) = f a <> foldMap f as

-- Construction Plan For Traverse
--
-- Start with a pure list:                                      (x1 : x2 : Nil)
-- Apply function, creating a list of applicatives:             (A(y1) : A(y2) : Nil)
-- Partially apply Cons:                                        (A(y1 : _) : A(y2 : _) : Nil)
-- Apply each element to each other in right-associative way:   A(y1 : _) <*> (A(y2 : _) <*> pure Nil)
-- Result:                                                      A(y1 : y2 : Nil)

instance Traversable Liste where
  traverse :: ∀ a b m. Applicative m => (a -> m b) -> Liste a -> m (Liste b)
  traverse f = foldr (\x acc -> (:) <$> f x <*> acc) (pure Nil)              -- <$>, <*> are both left-associative with same precedence
  sequence :: ∀ a m. Applicative m => Liste (m a) -> m (Liste a)
  sequence = traverse identity

half :: Int -> Maybe Int
half x = if even x then Just (x `div` 2) else Nothing

main :: Effect Unit
main = do
  log "-------------------------"
  log "-- Applicative Example --"
  log "-------------------------"
  log $ show $ fullNameEither (Just "Hans") (Just "Fritz") (Just "Schmid")   -- (Right "Hans Fritz Schmid")
  log $ show $ fullNameEither Nothing (Just "Fritz") (Just "Schmid")         -- (Left "First name missing.")
  log $ show $ fullNameEither (Just "Hans") Nothing (Just "Schmid")          -- (Left "Middle name missing.")
  log $ show $ fullNameEither (Just "Hans") (Just "Fritz") Nothing           -- (Left "Last name missing.")
  log "-------------------------"
  log "-- Traversable Example --"
  log "-------------------------"
  let
    pureListe = (2 : 4 : 6 : Nil)
    applicativeListe = map half pureListe
  log $ show $ pureListe                                                     -- [2 : 4 : 6 : Nil]
  log $ show $ applicativeListe                                              -- [(Just 1) : (Just 2) : (Just 3) : Nil]
  log $ show $ sequence applicativeListe                                     -- (Just [1 : 2 : 3 : Nil])
  log $ show $ traverse half pureListe                                       -- (Just [1 : 2 : 3 : Nil])

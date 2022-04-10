module Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

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

main :: Effect Unit
main = do
  log $ show $ fullNameEither (Just "Hans") (Just "Fritz") (Just "Schmid")
  log $ show $ fullNameEither Nothing (Just "Fritz") (Just "Schmid")
  log $ show $ fullNameEither (Just "Hans") Nothing (Just "Schmid")
  log $ show $ fullNameEither (Just "Hans") (Just "Fritz") Nothing

module Ch22h where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Data.String.Common (toUpper, toLower)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, forkAff)
import Effect.Aff.Bus (BusR, BusW)
import Effect.Aff.Bus as Bus
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

readAFile :: BusW String -> Aff Unit
readAFile writeFileBus = do
  result <- try $ readTextFile ASCII "somefile.txt"
  case result of
    Right text -> Bus.write text writeFileBus
    Left err -> log $ show err

processFile :: (String -> String) -> BusR String -> Aff Unit
processFile convert readFileBus = do
  text <- Bus.read readFileBus
  log $ convert text

test :: Effect Unit
test = launchAff_ do
  Tuple readFileBus writeFileBus <- Bus.split <$> Bus.make
  void $ forkAff $ processFile toUpper readFileBus
  void $ forkAff $ processFile toLower readFileBus
  void $ forkAff $ readAFile writeFileBus

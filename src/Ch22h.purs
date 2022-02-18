module Ch22h where

import Prelude
import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Data.String.Common (toUpper, toLower)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Bus (BusR, BusW)
import Effect.Aff.Bus as Bus
import Effect.Aff (Aff, launchAff_, forkAff)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

readAFile :: BusW String -> Aff Unit
readAFile fileBus = do
  result <- try $ readTextFile ASCII "somefile.txt"
  case result of
    Right text -> Bus.write text fileBus
    Left err -> log $ show err

processFile :: (String -> String) -> BusR String -> Aff Unit
processFile convert fileBus = do
  text <- Bus.read fileBus
  log $ convert text

test :: Effect Unit
test = launchAff_ do
  fileBus <- Bus.make
  let Tuple readBus writeBus = Bus.split fileBus
  void $ forkAff $ processFile toUpper readBus
  void $ forkAff $ processFile toLower readBus
  void $ forkAff $ readAFile writeBus

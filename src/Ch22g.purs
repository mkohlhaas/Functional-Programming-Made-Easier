module Ch22g where

import Prelude
import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, forkAff, delay)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

readAFileAfterTwoSeconds :: AVar String -> Aff Unit
readAFileAfterTwoSeconds fileAVar = do
  delay (Milliseconds 2000.0)
  result <- try $ readTextFile ASCII "somefile.txt"
  case result of
    Right text -> AVar.put text fileAVar
    Left err -> log $ "readAFileAfterTwoSeconds: " <> show err

processFile :: AVar String -> Aff Unit
processFile fileAVar = do
  text <- AVar.take fileAVar
  log $ "processFile: " <> text

test :: Effect Unit
test = launchAff_ do
  fileAVar <- AVar.empty
  void $ forkAff $ processFile fileAVar
  void $ forkAff $ readAFileAfterTwoSeconds fileAVar

module Ch22c where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

readAFile :: Aff Unit
readAFile = do
  -- result <- try $ readTextFile ASCII "somefile.txt"
  result <- try $ readTextFile ASCII "test.txt"
  case result of
    Right fileData -> log fileData
    Left err -> log $ show err

test :: Effect Unit
test = launchAff_ readAFile

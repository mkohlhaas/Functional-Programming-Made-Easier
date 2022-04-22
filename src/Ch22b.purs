module Ch22b where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

readAFile :: Aff Unit
readAFile = do
  -- text <- readTextFile ASCII "somefile.txt"
  text <- readTextFile ASCII "test.txt"
  log text

test :: Effect Unit
test = launchAff_ readAFile

module Ch22a where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (Error)
import Node.Encoding (Encoding(..))
import Node.FS.Async (readTextFile)

displayFile :: Either Error String -> Effect Unit
displayFile (Right fileData) = log fileData
displayFile (Left err) = log $ show err

test :: Effect Unit
test = do
  readTextFile ASCII "somefile.txt" displayFile

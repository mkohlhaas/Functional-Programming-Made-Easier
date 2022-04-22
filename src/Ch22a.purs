module Ch22a where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Async (Callback, readTextFile)

displayFile :: Callback String
displayFile (Left err) = log $ show err
displayFile (Right fileData) = log fileData

test :: Effect Unit
test = readTextFile ASCII "somefile.txt" displayFile

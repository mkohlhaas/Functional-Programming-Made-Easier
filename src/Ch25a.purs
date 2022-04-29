module Ch25a where

import Prelude

import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import DataTypesFF (ReversedTeacher, testTeacher)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Foreign.Generic (decodeJSON, encodeJSON)
import Foreign.Generic.Class (class Decode)
import Type.Proxy (Proxy(..))

processAjaxResult :: ∀ a. Show a => Decode a => Proxy a -> Either Ajax.Error (Ajax.Response String) -> String
processAjaxResult _ = case _ of
  Left err -> Ajax.printError err
  Right { body } ->
    case runExcept (decodeJSON body :: _ a) of
      Left err -> show err
      Right reversedRecord -> show reversedRecord

-------------------
-- Test Function --
-------------------

test :: Effect Unit
test = launchAff_ do
  result <- Ajax.post ResponseFormat.string "http://localhost:3000/" $ Just $ RequestBody.string $ encodeJSON testTeacher
  log $ processAjaxResult (Proxy :: _ ReversedTeacher) result

-- Output:
-- { lanosrep: { ega: 31, thgieh: 162.56, thgiew: 63.5 }, sedarg: [Preschool,Kindergarten,(Grade 1)], stnedutSfOrebmun: 23, sutats: NonTenured }

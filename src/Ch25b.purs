module Ch25b where

import Prelude

import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (class DecodeJson, Json, decodeJson, stringify)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import DataTypesFA (Teacher, testTeacher)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Foreign.Generic (encodeJSON)
import Type.Proxy (Proxy(..))

----------------------
-- Helper Functions --
----------------------
processAjaxResult :: ∀ a. Show a => DecodeJson a => Proxy a -> Either Ajax.Error (Ajax.Response Json) -> String
processAjaxResult _ = case _ of
  Left err -> Ajax.printError err
  Right { body } ->
    case decodeJson body :: _ a of
      Left err -> show err
      Right reversedRecord -> show reversedRecord

-------------------
-- Test Function --
-------------------
test :: Effect Unit
test = launchAff_ do
  result <- Ajax.post ResponseFormat.json "http://localhost:3000/" $ Just $ RequestBody.String $ encodeJSON testTeacher
  log $ show $ bimap Ajax.printError (stringify <<< _.body) $ result
  log $ processAjaxResult (Proxy :: _ Teacher) result

-- Output:
-- (Right "{\"stnetnoc\":{\"sutats\":{\"gat\":\"NonTenured\"},\"lanosrep\":{\"stnetnoc\":{\"thgiew\":63.5,\"thgieh\":162.56,\"ega\":31},\"gat\":\"Personal\"},\"stnedutSfOrebmun\":23,\"sedarg\":[{\"gat\":\"Preschool\"},{\"gat\":\"Kindergarten\"},{\"stnetnoc\":1,\"gat\":\"Grade\"}]},\"gat\":\"Teacher\"}")
-- (Teacher { grades: [Preschool,Kindergarten,(Grade 1)], numberOfStudents: 23, personal: (Personal { age: 31, height: 162.56, weight: 63.5 }), status: NonTenured })

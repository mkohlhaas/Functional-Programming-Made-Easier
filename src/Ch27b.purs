module Ch27b where

import Prelude

import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
import Control.Parallel (parSequence)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import DataTypesFF (Student, Teacher, testStudent, testTeacher)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Foreign (Foreign, MultipleErrors)
import Foreign.Generic (decodeJSON, encodeJSON)
import Foreign.Generic.Class (class Decode)
import Foreign.JSON (parseJSON)

foreign import _reverseKeys :: Foreign -> String

test :: Effect Unit
test = launchAff_ do
  results <- parSequence $ (\json -> Ajax.post ResponseFormat.string "http://localhost:3000/" $ Just $ RequestBody.String json) <$> [ encodeJSON testTeacher , encodeJSON testStudent ]
  log $ case map (_.body) <$> sequence results of
    Left err -> Ajax.printError err
    Right [ teacherJson, studentJson ] -> show (processJSON teacherJson :: _ Teacher) <> "\n" <> show (processJSON studentJson :: _ Student)
    Right _ -> "The number of Ajax calls is different than what's being processed."
  where
  processJSON :: ∀ a. Decode a => String -> Either MultipleErrors a
  processJSON json = runExcept do
    o <- parseJSON json
    decodeJSON $ _reverseKeys o

-- Output:
-- (Right (Teacher { grades: [Preschool,Kindergarten,(Grade 1)], numberOfStudents: 23, personal: (Personal { age: 31, height: 162.56, weight: 63.5 }), status: NonTenured }))
-- (Right (Student { gpa: 3.2, grade: (Grade 1), personal: (Personal { age: 5, height: 107.9, weight: 17.9 }), teacher: (Teacher { grades: [Preschool,Kindergarten,(Grade 1)], numberOfStudents: 23, personal: (Personal { age: 31, height: 162.56, weight: 63.5 }), status: NonTenured }) }))

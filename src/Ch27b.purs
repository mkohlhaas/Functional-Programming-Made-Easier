module Ch27b where

--------------------------------------------------------------------------------
-------------------------- Imports ---------------------------------------------
--------------------------------------------------------------------------------

import Prelude

import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
import Control.Parallel (parSequence)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Foreign (Foreign, MultipleErrors)
import Foreign.Generic (defaultOptions, encodeJSON, decodeJSON, genericEncode, genericDecode)
import Foreign.Generic.Class (class Decode, class Encode)
import Foreign.JSON (parseJSON)

foreign import _reverseKeys :: Foreign -> String

--------------------------------------------------------------------------------
-------------------------- Data Types ------------------------------------------
--------------------------------------------------------------------------------

newtype Centimeters = Centimeters Number
newtype Kilograms = Kilograms Number
newtype Years = Years Int
newtype GPA = GPA Number

newtype Personal = Personal
  { height :: Centimeters
  , weight :: Kilograms
  , age :: Years
  }

data Grade = Preschool | Kindergarten | Grade Int | High Int | College Int

newtype Student = Student
  { grade :: Grade
  , teacher :: Teacher
  , gpa :: GPA
  , personal :: Personal
  }

data TeachingStatus = StudentTeacher | Probationary | NonTenured | Tenured

newtype Teacher = Teacher
  { grades :: Array Grade
  , numberOfStudents :: Int
  , personal :: Personal
  , status :: TeachingStatus
  }

---------------
-- Test Data --
---------------

testTeacher :: Teacher
testTeacher = Teacher
  { grades: [ Preschool, Kindergarten, Grade 1 ]
  , numberOfStudents: 23
  , personal: Personal
      { height: Centimeters 162.56
      , weight: Kilograms 63.5
      , age: Years 31
      }
  , status: NonTenured
  }

testStudent :: Student
testStudent = Student
  { grade: Grade 1
  , teacher: testTeacher
  , gpa: GPA 3.2
  , personal: Personal
      { height: Centimeters 107.9
      , weight: Kilograms 17.9
      , age: Years 5
      }
  }

--------------------------------------------------------------------------------
--------------------- Instances and Instance Derivations -----------------------
--------------------------------------------------------------------------------

-----------------
-- Centimeters --
-----------------

derive instance genericCentimeters :: Generic Centimeters _
derive newtype instance encodeCentimeters :: Encode Centimeters
derive newtype instance decodeCentimeters :: Decode Centimeters
derive newtype instance showCentimeters :: Show Centimeters

---------------
-- Kilograms --
---------------

derive instance genericKilograms :: Generic Kilograms _
derive newtype instance encodeKilograms :: Encode Kilograms
derive newtype instance decodeKilograms :: Decode Kilograms
derive newtype instance showKilograms :: Show Kilograms

-----------
-- Years --
-----------

derive instance genericYears :: Generic Years _
derive newtype instance encodeYears :: Encode Years
derive newtype instance decodeYears :: Decode Years
derive newtype instance showYears :: Show Years

---------
-- GPA --
---------

derive instance genericGPA :: Generic GPA _
derive newtype instance encodeGPA :: Encode GPA
derive newtype instance decodeGPA :: Decode GPA
derive newtype instance showGPA :: Show GPA

-----------
-- Grade --
-----------

derive instance genericGrade :: Generic Grade _

instance showGrade :: Show Grade where
  show = genericShow

instance decodeGrade :: Decode Grade where
  decode = genericDecode defaultOptions

instance encodeGrade :: Encode Grade where
  encode = genericEncode defaultOptions

--------------------
-- TeachingStatus --
--------------------

derive instance genericTeachingStatus :: Generic TeachingStatus _

instance showTeachingStatus :: Show TeachingStatus where
  show = genericShow

instance encodeTeachingStatus :: Encode TeachingStatus where
  encode = genericEncode defaultOptions

instance decodeTeachingStatus :: Decode TeachingStatus where
  decode = genericDecode defaultOptions

--------------
-- Personal --
--------------

derive instance genericPersonal :: Generic Personal _

instance showPersonal :: Show Personal where
  show = genericShow

instance decodePersonal :: Decode Personal where
  decode = genericDecode defaultOptions

instance encodePersonal :: Encode Personal where
  encode = genericEncode defaultOptions

-------------
-- Student --
-------------

derive instance genericStudent :: Generic Student _

instance showStudent :: Show Student where
  show = genericShow

instance decodeStudent :: Decode Student where
  decode = genericDecode defaultOptions

instance encodeStudent :: Encode Student where
  encode = genericEncode defaultOptions

-------------
-- Teacher --
-------------

derive instance genericTeacher :: Generic Teacher _

instance showTeacher :: Show Teacher where
  show = genericShow

instance decodeTeacher :: Decode Teacher where
  decode = genericDecode defaultOptions

instance encodeTeacher :: Encode Teacher where
  encode = genericEncode defaultOptions

--------------------------------------------------------------------------------
--------------------------  Test Function --------------------------------------
--------------------------------------------------------------------------------

test :: Effect Unit
test = launchAff_ do
  results <- parSequence $ (\json -> Ajax.post ResponseFormat.string "http://localhost:3000/" $ Just $ RequestBody.String json) <$>
    [ encodeJSON testTeacher
    , encodeJSON testStudent
    ]
  log $ case map (_.body) <$> sequence results of
    Left err -> Ajax.printError err
    Right [ teacherJson, studentJson ] -> show (processJSON teacherJson :: _ Teacher) <> "\n\n" <> show (processJSON studentJson :: _ Student)
    Right _ -> "The number of Ajax calls is different than what's being processed."
  where
  processJSON :: ∀ a. Decode a => String -> Either MultipleErrors a
  processJSON json = runExcept do
    o <- parseJSON json
    decodeJSON $ _reverseKeys o

module Ch25b where

import Prelude

import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
-- import Control.Monad.Except (runExcept)
import Data.Argonaut (class DecodeJson, decodeJson, Json, JsonDecodeError(..), (.:))
import Data.Argonaut.Decode.Decoders (decodeJObject)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Foreign.Generic (defaultOptions, encodeJSON, genericDecode, genericEncode)
import Foreign.Generic.Class (class Decode, class Encode)
import Type.Proxy (Proxy(..))

-- import Data.Bifunctor (bimap)
import Data.Maybe (Maybe(..))
-- import Effect.Aff (launchAff_)
-- import Web.DOM.Document (doctype)

----------------
-- Data Types --
----------------

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

----------------------------------------
-- Instances and Instance Derivations --
----------------------------------------

-- Centimeters --
derive instance genericCentimeters :: Generic Centimeters _
derive newtype instance encodeCentimeters :: Encode Centimeters
derive newtype instance decodeCentimeters :: Decode Centimeters
derive newtype instance decodeJsonCentimeters :: DecodeJson Centimeters
derive newtype instance showCentimeters :: Show Centimeters

-- Kilograms --
derive instance genericKilograms :: Generic Kilograms _
derive newtype instance encodeKilograms :: Encode Kilograms
derive newtype instance decodeKilograms :: Decode Kilograms
derive newtype instance decodeJsonKilograms :: DecodeJson Kilograms
derive newtype instance showKilograms :: Show Kilograms

-- Years --
derive instance genericYears :: Generic Years _
derive newtype instance encodeYears :: Encode Years
derive newtype instance decodeYears :: Decode Years
derive newtype instance decodeJsonYears :: DecodeJson Years
derive newtype instance showYears :: Show Years

-- GPA --
derive instance genericGPA :: Generic GPA _
derive newtype instance encodeGPA :: Encode GPA
derive newtype instance decodeGPA :: Decode GPA
derive newtype instance decodeJsonGPA :: DecodeJson GPA
derive newtype instance showGPA :: Show GPA

-- Grade --
derive instance genericGrade :: Generic Grade _

instance showGrade :: Show Grade where
  show = genericShow

instance encodeGrade :: Encode Grade where
  encode = genericEncode defaultOptions

instance decodeGrade :: Decode Grade where
  decode = genericDecode defaultOptions

instance decodeJsonGrade :: DecodeJson Grade where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    let
      contents :: ∀ a. DecodeJson a => Either JsonDecodeError a
      contents = o .: "stnetnoc"
    case tag of
      "Preschool" -> pure Preschool
      "Kindergarten" -> pure Kindergarten
      "Grade" -> Grade <$> contents
      "High" -> High <$> contents
      "College" -> College <$> contents
      _ -> Left $ AtKey "tag" $ UnexpectedValue json

-- TeachingStatus --
derive instance genericTeachingStatus :: Generic TeachingStatus _

instance showTeachingStatus :: Show TeachingStatus where
  show = genericShow

instance encodeTeachingStatus :: Encode TeachingStatus where
  encode = genericEncode defaultOptions

instance decodeTeachingStatus :: Decode TeachingStatus where
  decode = genericDecode defaultOptions

instance decodeJsonTeachingStatus :: DecodeJson TeachingStatus where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    case tag of
      "StudentTeacher" -> pure StudentTeacher
      "Probationary" -> pure Probationary
      "NonTenured" -> pure NonTenured
      "Tenured" -> pure Tenured
      _ -> Left $ AtKey "tag" $ UnexpectedValue json

-- Personal --
derive instance genericPersonal :: Generic Personal _
derive newtype instance encodePersonal :: Encode Personal
derive newtype instance decodePersonal :: Decode Personal
-- derive newtype instance decodeJsonPersonal :: DecodeJson Personal
derive newtype instance showPersonal :: Show Personal

instance decodeJsonPersonal :: DecodeJson Personal where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    if tag == "Personal" then do
      height <- o .: "thgieh"
      weight <- o .: "thgiew"
      age <- o .: "ega"
      pure $ Personal { height, weight, age }
    else Left $ AtKey "tag" $ UnexpectedValue json

-- Student --
derive instance genericStudent :: Generic Student _
derive newtype instance encodeStudent :: Encode Student
derive newtype instance decodeStudent :: Decode Student
-- derive newtype instance decodeJsonStudent :: DecodeJson Student
derive newtype instance showStudent :: Show Student

instance decodeJsonStudent :: DecodeJson Student where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    if tag == "Student" then do
      grade <- o .: "edarg"
      teacher <- o .: "rehcaet" -- COMPILER WARNING!!
      gpa <- o .: "apg"
      personal <- o .: "lanosrep"
      pure $ Student { grade, teacher, gpa, personal }
    else Left $ AtKey "tag" $ UnexpectedValue json

-- Teacher --
derive instance genericTeacher :: Generic Teacher _
derive newtype instance encodeTeacher :: Encode Teacher
derive newtype instance decodeTeacher :: Decode Teacher
-- derive newtype instance decodeJsonTeacher :: DecodeJson Teacher
derive newtype instance showTeacher :: Show Teacher

instance decodeJsonTeacher :: DecodeJson Teacher where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    if tag == "Teacher" then do
      grades <- o .: "sedarg"
      numberOfStudents <- o .: "stnedutSfOrebmun"
      personal <- o .: "lanosrep"
      status <- o .: "sutats"
      pure $ Teacher { grades, numberOfStudents, personal, status }
    else Left $ AtKey "tag" $ UnexpectedValue json

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
  log $ processAjaxResult (Proxy :: _ Teacher) result

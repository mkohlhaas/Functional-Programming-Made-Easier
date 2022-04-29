module DataTypesFA where

-- FA = encode Foreign, decode Argonaut

import Prelude

import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), decodeJson, (.:))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign.Generic (defaultOptions, genericEncode)
import Foreign.Generic.Class (class Encode)

----------------------------------------------------
-- Data Types - all types are 'newtype' or 'data' --
----------------------------------------------------
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

-----------------
-- Centimeters --
-----------------
derive instance Generic Centimeters _
derive newtype instance Encode Centimeters
derive newtype instance DecodeJson Centimeters
derive newtype instance Show Centimeters

---------------
-- Kilograms --
---------------
derive instance Generic Kilograms _
derive newtype instance Encode Kilograms
derive newtype instance DecodeJson Kilograms
derive newtype instance Show Kilograms

-----------
-- Years --
-----------
derive instance Generic Years _
derive newtype instance Encode Years
derive newtype instance DecodeJson Years
derive newtype instance Show Years

---------
-- GPA --
---------
derive instance Generic GPA _
derive newtype instance Encode GPA
derive newtype instance DecodeJson GPA
derive newtype instance Show GPA

-----------
-- Grade --
-----------
derive instance Generic Grade _
instance Show Grade where
  show = genericShow

instance Encode Grade where
  encode = genericEncode defaultOptions

--------------------
-- TeachingStatus --
--------------------
derive instance Generic TeachingStatus _
instance Show TeachingStatus where
  show = genericShow

instance Encode TeachingStatus where
  encode = genericEncode defaultOptions

--------------
-- Personal --
--------------
derive instance Generic Personal _
instance Show Personal where
  show = genericShow

instance Encode Personal where
  encode = genericEncode defaultOptions

-------------
-- Student --
-------------
derive instance Generic Student _
instance Show Student where
  show = genericShow

instance Encode Student where
  encode = genericEncode defaultOptions

-------------
-- Teacher --
-------------
derive instance Generic Teacher _
instance Show Teacher where
  show = genericShow

instance Encode Teacher where
  encode = genericEncode defaultOptions

---------------------------------
-- Argonaut Decoding Instances --
---------------------------------
instance DecodeJson Grade where
  decodeJson json = do
    o <- decodeJson json
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
      _ -> fail json

instance DecodeJson TeachingStatus where
  decodeJson json = do
    o <- decodeJson json
    tag <- o .: "gat"
    case tag of
      "StudentTeacher" -> pure StudentTeacher
      "Probationary" -> pure Probationary
      "NonTenured" -> pure NonTenured
      "Tenured" -> pure Tenured
      _ -> fail json

instance DecodeJson Personal where
  decodeJson json = do
    o <- decodeJson json
    tag <- o .: "gat"
    if tag == "Personal" then do
      c <- o .: "stnetnoc"
      height <- c .: "thgieh"
      weight <- c .: "thgiew"
      age <- c .: "ega"
      pure $ Personal { height, weight, age }
    else fail json

instance DecodeJson Student where
  decodeJson json = do
    o <- decodeJson json
    tag <- o .: "gat"
    if tag == "Student" then do
      c <- o .: "stnetnoc"
      grade <- c .: "edarg"
      teacher <- c .: "rehcaet"
      gpa <- c .: "apg"
      personal <- c .: "lanosrep"
      pure $ Student { grade, teacher, gpa, personal }
    else fail json

instance DecodeJson Teacher where
  decodeJson json = do
    o <- decodeJson json
    tag <- o .: "gat"
    if tag == "Teacher" then do
      c <- o .: "stnetnoc"
      grades <- c .: "sedarg"
      numberOfStudents <- c .: "stnedutSfOrebmun"
      personal <- c .: "lanosrep"
      status <- c .: "sutats"
      pure $ Teacher { grades, numberOfStudents, personal, status }
    else fail json

fail :: ∀ a. Json -> Either JsonDecodeError a
fail = Left <<< AtKey "tag" <<< UnexpectedValue

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

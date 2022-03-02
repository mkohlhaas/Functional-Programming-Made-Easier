module Ch25a where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Control.Monad.Except (runExcept)
import Data.Either
import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Foreign.Generic (encodeJSON, genericEncode, genericDecode, defaultOptions, decodeJSON)
import Foreign.Generic.Class (class Decode, class Encode, Options, SumEncoding(..))
import Affjax as Ajax
import Affjax.ResponseFormat as ResponseFormat
import Affjax.RequestBody as RequestBody
import Effect.Aff (launchAff_)
import Type.Proxy (Proxy(..))

-- import Data.Bifunctor (bimap)

----------------
-- Data Types --
----------------

newtype Centimeters = Centimeters Number
newtype Kilograms = Kilograms Number
newtype Years = Years Int
newtype GPA = GPA Number

type Personal =
  { height :: Centimeters
  , weight :: Kilograms
  , age :: Years
  }

data Grade = Preschool | Kindergarten | Grade Int | High Int | College Int

type Student =
  { grade :: Grade
  , teacher :: Teacher
  , gpa :: GPA
  , personal :: Personal
  }

data TeachingStatus = Student | Probationary | NonTenured | Tenured

type Teacher =
  { grades :: Array Grade
  , numberOfStudents :: Int
  , personal :: Personal
  , status :: TeachingStatus
  }

type ReversedPersonal =
  { thgieh :: Centimeters
  , thgiew :: Kilograms
  , ega :: Years
  }

type ReversedStudent =
  { edarg :: Grade
  , rehcaet :: ReversedTeacher
  , apg :: GPA
  , lanosrep :: ReversedPersonal
  }

type ReversedTeacher =
  { sedarg :: Array Grade
  , stnedutSfOrebmun :: Int
  , lanosrep :: ReversedPersonal
  , sutats :: TeachingStatus
  }

---------------
-- Test Data --
---------------

teacher :: Teacher
teacher =
  { grades: [ Preschool, Kindergarten, Grade 1 ]
  , numberOfStudents: 23
  , personal:
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
derive newtype instance showCentimeters :: Show Centimeters

-- Kilograms --
derive instance genericKilograms :: Generic Kilograms _
derive newtype instance encodeKilograms :: Encode Kilograms
derive newtype instance decodeKilograms :: Decode Kilograms
derive newtype instance showKilograms :: Show Kilograms

-- Years --
derive instance genericYears :: Generic Years _
derive newtype instance encodeYears :: Encode Years
derive newtype instance decodeYears :: Decode Years
derive newtype instance showYears :: Show Years

-- GPA --
derive instance genericGPA :: Generic GPA _
derive newtype instance encodeGPA :: Encode GPA
derive newtype instance decodeGPA :: Decode GPA
derive newtype instance showGPA :: Show GPA

-- Grade --
derive instance genericGrade :: Generic Grade _

instance showGrade :: Show Grade where
  show = genericShow

instance encodeGrade :: Encode Grade where
  encode = genericEncode defaultOptions

instance decodeGrade :: Decode Grade where
  decode = genericDecode decodeOptions

-- TeachingStatus --
derive instance genericTeachingStatus :: Generic TeachingStatus _

instance showTeachingStatus :: Show TeachingStatus where
  show = genericShow

instance encodeTeachingStatus :: Encode TeachingStatus where
  encode = genericEncode defaultOptions

instance decodeTeachingStatus :: Decode TeachingStatus where
  decode = genericDecode decodeOptions

----------------------
-- Helper Functions --
----------------------

decodeOptions :: Options
decodeOptions = defaultOptions
  { sumEncoding = TaggedObject
      { tagFieldName: "gat"
      , contentsFieldName: "stnetnoc"
      , constructorTagTransform: identity
      }
  }

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
  result <- Ajax.post ResponseFormat.string "http://localhost:8000/" $ Just $ RequestBody.string $ encodeJSON teacher
  log $ processAjaxResult (Proxy:: _ ReversedTeacher) result

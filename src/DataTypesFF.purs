module DataTypesFF where

-- FF = encode Foreign, decode Foreign

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.Class (class Decode, class Encode, Options, SumEncoding(..))

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

----------------------------------------
-- Instances and Instance Derivations --
----------------------------------------

-- Centimeters --
derive instance Generic Centimeters _
derive newtype instance Encode Centimeters
derive newtype instance Decode Centimeters
derive newtype instance Show Centimeters

-- Kilograms --
derive instance Generic Kilograms _
derive newtype instance Encode Kilograms
derive newtype instance Decode Kilograms
derive newtype instance Show Kilograms

-- Years --
derive instance Generic Years _
derive newtype instance Encode Years
derive newtype instance Decode Years
derive newtype instance Show Years

-- GPA --
derive instance Generic GPA _
derive newtype instance Encode GPA
derive newtype instance Decode GPA
derive newtype instance Show GPA

-- Grade --
derive instance Generic Grade _
instance Show Grade where
  show = genericShow

instance Encode Grade where
  encode = genericEncode defaultOptions

instance Decode Grade where
  decode = genericDecode decodeOptions

-- TeachingStatus --
derive instance Generic TeachingStatus _
instance Show TeachingStatus where
  show = genericShow

instance Encode TeachingStatus where
  encode = genericEncode defaultOptions

instance Decode TeachingStatus where
  decode = genericDecode decodeOptions

decodeOptions :: Options
decodeOptions = defaultOptions
  { sumEncoding = TaggedObject
      { tagFieldName: "gat"
      , contentsFieldName: "stnetnoc"
      , constructorTagTransform: identity
      }
  }

---------------
-- Test Data --
---------------
testTeacher :: Teacher
testTeacher =
  { grades: [ Preschool, Kindergarten, Grade 1 ]
  , numberOfStudents: 23
  , personal:
      { height: Centimeters 162.56
      , weight: Kilograms 63.5
      , age: Years 31
      }
  , status: NonTenured
  }

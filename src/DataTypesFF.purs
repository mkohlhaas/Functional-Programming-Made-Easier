module DataTypesFF where

-- FF = encode Foreign, decode Foreign

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.Class (class Decode, class Encode)

---------------
-- Data Typs --
---------------
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
-- Instances --
---------------
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

instance Decode Grade where
  decode = genericDecode defaultOptions

instance Encode Grade where
  encode = genericEncode defaultOptions

-- TeachingStatus --
derive instance Generic TeachingStatus _
instance Show TeachingStatus where
  show = genericShow

instance Encode TeachingStatus where
  encode = genericEncode defaultOptions

instance Decode TeachingStatus where
  decode = genericDecode defaultOptions

-- Personal --
derive instance Generic Personal _
instance Show Personal where
  show = genericShow

instance Decode Personal where
  decode = genericDecode defaultOptions

instance Encode Personal where
  encode = genericEncode defaultOptions

-- Student --
derive instance Generic Student _
instance Show Student where
  show = genericShow

instance Decode Student where
  decode = genericDecode defaultOptions

instance Encode Student where
  encode = genericEncode defaultOptions

-- Teacher --
derive instance Generic Teacher _
instance Show Teacher where
  show = genericShow

instance Decode Teacher where
  decode = genericDecode defaultOptions

instance Encode Teacher where
  encode = genericEncode defaultOptions

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

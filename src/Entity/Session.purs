module Entity.Session where

import Data.Generic.Rep (class Generic)
import Data.UUID (UUID)
import Foreign.Generic (genericEncode, genericDecode)
import Foreign.Generic.Class (class Encode, class Decode, defaultOptions)

newtype Session = Session
  { authToken :: UUID
  , userName :: String
  , lastTime :: Number
  }

derive instance Generic Session _
instance Encode Session where
  encode = genericEncode defaultOptions

instance Decode Session where
  decode = genericDecode defaultOptions

module Entity.Account where

import Data.Generic.Rep (class Generic)
import Entity.User (UserRow)
import Foreign.Generic (genericEncode, genericDecode)
import Foreign.Generic.Class (class Encode, class Decode, defaultOptions)

newtype Account = Account (Record (UserRow (passwordHash :: String)))

derive instance Generic Account _
instance Encode Account where
  encode = genericEncode defaultOptions

instance Decode Account where
  decode = genericDecode defaultOptions

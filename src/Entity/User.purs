module Entity.User where

import Data.Generic.Rep (class Generic)
import Foreign.Generic (genericEncode, genericDecode)
import Foreign.Generic.Class (class Encode, class Decode, defaultOptions)

type UserRow r =
  ( userName :: String
  , temporaryPassword :: Boolean
  , admin :: Boolean
  , firstName :: String
  , lastName :: String
  | r
  )

newtype User = User (Record (UserRow ()))

-- Alternative syntax:
-- newtype User = User { | UserRow () }

derive instance Generic User _
instance Encode User where
  encode = genericEncode defaultOptions

instance Decode User where
  decode = genericDecode defaultOptions

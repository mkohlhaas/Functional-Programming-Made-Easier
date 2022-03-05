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

-- newtype User = User { | UserRow () }
newtype User = User (Record (UserRow ()))

-- newtype User = User
--   { userName :: String
--   , temporaryPassword :: Boolean
--   , admin :: Boolean
--   , firstName :: String
--   , lastName :: String
--   }

derive instance genericUser :: Generic User _

instance encodeUser :: Encode User where
  encode = genericEncode defaultOptions

instance decodeUser :: Decode User where
  decode = genericDecode defaultOptions

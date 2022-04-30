module Data.Api.Logon where

import Data.Generic.Rep (class Generic)
import Data.UUID (UUID)
import Foreign.Generic (genericEncode, genericDecode)
import Foreign.Generic.Class (class Encode, class Decode, defaultOptions)

----------------
-- Data Types --
----------------
newtype LogonRequest = LogonRequest
  { userName :: String
  , password :: String
  }

newtype LogonResponse = LogonResponse LogonResults

data LogonResults
  = LogonResultsSuccess
      { authToken :: UUID
      , mustChangePassword :: Boolean
      }
  | LogonResultsFailure

------------------
-- LogonRequest --
------------------
derive instance Generic LogonRequest _
instance Encode LogonRequest where
  encode = genericEncode defaultOptions

instance Decode LogonRequest where
  decode = genericDecode defaultOptions

------------------
-- LogonResults --
------------------
derive instance Generic LogonResults _
instance Encode LogonResults where
  encode = genericEncode defaultOptions

instance Decode LogonResults where
  decode = genericDecode defaultOptions

-------------------
-- LogonResponse --
-------------------
derive instance Generic LogonResponse _
instance Encode LogonResponse where
  encode = genericEncode defaultOptions

instance Decode LogonResponse where
  decode = genericDecode defaultOptions

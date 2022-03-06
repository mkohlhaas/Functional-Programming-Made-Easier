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

data LogonResults
  = LogonResultsSuccess
      { authToken :: UUID
      , mustChangePassword :: Boolean
      }
  | LogonResultsFailure

newtype LogonResponse = LogonResponse LogonResults

----------------------------------------
-- Instances and Instance Derivations --
----------------------------------------

------------------
-- LogonRequest --
------------------
derive instance genericLogonRequest :: Generic LogonRequest _

instance encodeLogonRequest :: Encode LogonRequest where
  encode = genericEncode defaultOptions

instance decodeLogonRequest :: Decode LogonRequest where
  decode = genericDecode defaultOptions

------------------
-- LogonResults --
------------------

derive instance genericLogonResults :: Generic LogonResults _

instance encodeLogonResults :: Encode LogonResults where
  encode = genericEncode defaultOptions

instance decodeLogonResults :: Decode LogonResults where
  decode = genericDecode defaultOptions

-------------------
-- LogonResponse --
-------------------

derive instance genericLogonResponse :: Generic LogonResponse _

instance encodeLogonResponse :: Encode LogonResponse where
  encode = genericEncode defaultOptions

instance decodeLogonResponse :: Decode LogonResponse where
  decode = genericDecode defaultOptions

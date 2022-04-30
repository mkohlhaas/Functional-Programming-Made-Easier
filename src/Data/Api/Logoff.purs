module Data.Api.Logoff where

import Data.Generic.Rep (class Generic)
import Data.UUID (UUID)
import Foreign.Generic (genericEncode, genericDecode)
import Foreign.Generic.Class (class Encode, class Decode, defaultOptions)

----------------
-- Data Types --
----------------
newtype LogoffRequest = LogoffRequest
  { authToken :: UUID }

newtype LogoffResponse = LogoffResponse LogoffResult

data LogoffResult
  = LogoffResultsFailure
  | LogoffResultsSuccess

-------------------
-- LogoffRequest --
-------------------
derive instance Generic LogoffRequest _
instance Encode LogoffRequest where
  encode = genericEncode defaultOptions

instance Decode LogoffRequest where
  decode = genericDecode defaultOptions

------------------
-- LogoffResult --
------------------
derive instance Generic LogoffResult _
instance Encode LogoffResult where
  encode = genericEncode defaultOptions

instance Decode LogoffResult where
  decode = genericDecode defaultOptions

--------------------
-- LogoffResponse --
--------------------
derive instance Generic LogoffResponse _
instance Encode LogoffResponse where
  encode = genericEncode defaultOptions

instance Decode LogoffResponse where
  decode = genericDecode defaultOptions

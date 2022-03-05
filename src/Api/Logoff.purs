module Api.Logoff where

import Data.Generic.Rep (class Generic)
import Data.UUID (UUID)
import Foreign.Generic (genericEncode, genericDecode)
import Foreign.Generic.Class (class Encode, class Decode, defaultOptions)

----------------
-- Data Types --
----------------

newtype LogoffRequest = LogoffRequest
  { authToken :: UUID }

data LogoffResult
  = LogoffResultsFailure
  | LogoffResultsSuccess

newtype LogoffResponse = LogoffResponse LogoffResult

----------------------------------------
-- Instances and Instance Derivations --
----------------------------------------

-------------------
-- LogoffRequest --
-------------------

derive instance genericLogoffRequest :: Generic LogoffRequest _

instance encodeLogoffRequest :: Encode LogoffRequest where
  encode = genericEncode defaultOptions

instance decodeLogoffRequest :: Decode LogoffRequest where
  decode = genericDecode defaultOptions

------------------
-- LogoffResult --
------------------

derive instance genericLogoffResult :: Generic LogoffResult _

instance encodeLogoffResult :: Encode LogoffResult where
  encode = genericEncode defaultOptions

instance decodeLogoffResult :: Decode LogoffResult where
  decode = genericDecode defaultOptions

--------------------
-- LogoffResponse --
--------------------

derive instance genericLogoffResponse :: Generic LogoffResponse _

instance encodeLogoffResponse :: Encode LogoffResponse where
  encode = genericEncode defaultOptions

instance decodeLogoffResponse :: Decode LogoffResponse where
  decode = genericDecode defaultOptions

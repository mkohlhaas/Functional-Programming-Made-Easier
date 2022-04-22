module Data.Api.CreateUser where

import Data.Generic.Rep (class Generic)
import Data.UUID (UUID)
import Entity.User (UserRow)
import Foreign.Generic (genericEncode, genericDecode)
import Foreign.Generic.Class (class Encode, class Decode, defaultOptions)

----------------
-- Data Types --
----------------

newtype CreateUserRequest = CreateUserRequest { authToken :: UUID, user :: Record (UserRow (password :: String)) }

data CreateUserResults
  = CreateUserResultsSuccess
  | CreateUserResultsFailure { reason :: CreateUserFailureReason }

newtype CreateUserResponse = CreateUserResponse CreateUserResults

data CreateUserFailureReason
  = AlreadyExists
  | NotAuthenticated
  | NotAuthorized
  | FileIOError String

----------------------------------------
-- Instances and Instance Derivations --
----------------------------------------

-----------------------
-- CreateUserRequest --
-----------------------

derive instance genericCreateUserRequest :: Generic CreateUserRequest _

instance encodeCreateUserRequest :: Encode CreateUserRequest where
  encode = genericEncode defaultOptions

instance decodeCreateUserRequest :: Decode CreateUserRequest where
  decode = genericDecode defaultOptions

-------------------------------
-- CreateUserResponseResults --
-------------------------------

derive instance genericCreateUserResults :: Generic CreateUserResults _

instance encodeCreateUserResults :: Encode CreateUserResults where
  encode = genericEncode defaultOptions

instance decodeCreateUserResults :: Decode CreateUserResults where
  decode = genericDecode defaultOptions

------------------------
-- CreateUserResponse --
------------------------

derive instance genericCreateUserResponse :: Generic CreateUserResponse _

instance encodeCreateUserResponse :: Encode CreateUserResponse where
  encode = genericEncode defaultOptions

instance decodeCreateUserResponse :: Decode CreateUserResponse where
  decode = genericDecode defaultOptions

------------------------------------
-- CreateUserResultsFailureReason --
------------------------------------

derive instance genericCreateUserFailureReason :: Generic CreateUserFailureReason _

instance encodeCreateUserFailureReason :: Encode CreateUserFailureReason where
  encode = genericEncode defaultOptions

instance decodeCreateUserFailureReason :: Decode CreateUserFailureReason where
  decode = genericDecode defaultOptions

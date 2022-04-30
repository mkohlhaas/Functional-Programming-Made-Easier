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

newtype CreateUserResponse = CreateUserResponse CreateUserResults

data CreateUserResults
  = CreateUserResultsSuccess
  | CreateUserResultsFailure { reason :: CreateUserFailureReason }

data CreateUserFailureReason
  = AlreadyExists
  | NotAuthenticated
  | NotAuthorized
  | FileIOError String

-----------------------
-- CreateUserRequest --
-----------------------
derive instance Generic CreateUserRequest _
instance Encode CreateUserRequest where
  encode = genericEncode defaultOptions

instance Decode CreateUserRequest where
  decode = genericDecode defaultOptions

-------------------------------
-- CreateUserResponseResults --
-------------------------------
derive instance Generic CreateUserResults _
instance Encode CreateUserResults where
  encode = genericEncode defaultOptions

instance Decode CreateUserResults where
  decode = genericDecode defaultOptions

------------------------
-- CreateUserResponse --
------------------------
derive instance Generic CreateUserResponse _
instance Encode CreateUserResponse where
  encode = genericEncode defaultOptions

instance Decode CreateUserResponse where
  decode = genericDecode defaultOptions

------------------------------------
-- CreateUserResultsFailureReason --
------------------------------------
derive instance Generic CreateUserFailureReason _
instance Encode CreateUserFailureReason where
  encode = genericEncode defaultOptions

instance Decode CreateUserFailureReason where
  decode = genericDecode defaultOptions

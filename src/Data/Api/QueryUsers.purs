module Data.Api.QueryUsers where

import Data.Generic.Rep (class Generic)
import Data.UUID (UUID)
import Entity.User (User)
import Foreign.Generic (genericEncode, genericDecode)
import Foreign.Generic.Class (class Encode, class Decode, defaultOptions)

----------------
-- Data Types --
----------------
newtype QueryUsersRequest = QueryUsersRequest { authToken :: UUID }

newtype QueryUsersResponse = QueryUsersResponse QueryUsersResults

data QueryUsersResults
  = QueryUsersResultsSuccess { users :: Array User }
  | QueryUsersResultsFailure { reason :: QueryUsersFailureReason }

data QueryUsersFailureReason = NotAuthorized | NotAuthenticated

-----------------------
-- QueryUsersRequest --
-----------------------
derive instance Generic QueryUsersRequest _
instance Encode QueryUsersRequest where
  encode = genericEncode defaultOptions

instance Decode QueryUsersRequest where
  decode = genericDecode defaultOptions

------------------------
-- QueryUsersResponse --
------------------------
derive instance Generic QueryUsersResponse _
instance Encode QueryUsersResponse where
  encode = genericEncode defaultOptions

instance Decode QueryUsersResponse where
  decode = genericDecode defaultOptions

-----------------------
-- QueryUsersResults --
-----------------------
derive instance Generic QueryUsersResults _
instance Encode QueryUsersResults where
  encode = genericEncode defaultOptions

instance Decode QueryUsersResults where
  decode = genericDecode defaultOptions

-----------------------------
-- QueryUsersFailureReason --
-----------------------------
derive instance Generic QueryUsersFailureReason _
instance Encode QueryUsersFailureReason where
  encode = genericEncode defaultOptions

instance Decode QueryUsersFailureReason where
  decode = genericDecode defaultOptions

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

----------------------------------------
-- Instances and Instance Derivations --
----------------------------------------

-----------------------
-- QueryUsersRequest --
-----------------------

derive instance genericQueryUsersRequest :: Generic QueryUsersRequest _

instance encodeQueryUsersRequest :: Encode QueryUsersRequest where
  encode = genericEncode defaultOptions

instance decodeQueryUsersRequest :: Decode QueryUsersRequest where
  decode = genericDecode defaultOptions

------------------------
-- QueryUsersResponse --
------------------------

derive instance genericQueryUsersResponse :: Generic QueryUsersResponse _

instance encodeQueryUsersResponse :: Encode QueryUsersResponse where
  encode = genericEncode defaultOptions

instance decodeQueryUsersResponse :: Decode QueryUsersResponse where
  decode = genericDecode defaultOptions

-----------------------
-- QueryUsersResults --
-----------------------

derive instance genericQueryUsersResults :: Generic QueryUsersResults _

instance encodeQueryUsersResults :: Encode QueryUsersResults where
  encode = genericEncode defaultOptions

instance decodeQueryUsersResults :: Decode QueryUsersResults where
  decode = genericDecode defaultOptions

-----------------------------
-- QueryUsersFailureReason --
-----------------------------

derive instance genericQueryUsersFailureReason :: Generic QueryUsersFailureReason _

instance encodeQueryUsersFailureReason :: Encode QueryUsersFailureReason where
  encode = genericEncode defaultOptions

instance decodeQueryUsersFailureReason :: Decode QueryUsersFailureReason where
  decode = genericDecode defaultOptions

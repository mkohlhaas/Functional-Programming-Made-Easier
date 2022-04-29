module Main where

import Prelude

import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Foreign (F)
import Foreign.Generic (decodeJSON, encodeJSON)
import Foreign.Generic.Class (class Decode)
import Type.Proxy (Proxy(..))

----------
-- Ajax --
----------
type CreateBlogPostReq =
  { title :: String
  , body :: String
  , userId :: Int
  }

type GetPostRes =
  { id :: Int
  , title :: String
  , body :: String
  , userId :: Int
  }

type GetPostResList = Array GetPostRes

type CreateBlogPostRes =
  { id :: Int }

processAjaxResult :: ∀ m a. MonadEffect m => Decode a => Show a => Proxy a -> Either Ajax.Error (Ajax.Response String) -> m Unit
processAjaxResult _ = case _ of
  Left err -> log $ "ERROR: " <> Ajax.printError err
  Right { body } ->
    case runExcept (decodeJSON body :: F a) of
      Left err -> log $ "ERROR: " <> show err <> " (" <> body <> ")"
      Right v -> log $ show v

----------
-- Meal --
----------
newtype Meal = Meal { main :: String, side :: String, dessert :: String }

derive newtype instance EncodeJson Meal
derive newtype instance DecodeJson Meal
derive newtype instance Show Meal

meal :: String
meal = case Meal { main: "Main", side: "Side", dessert: "Dessert" } # encodeJson # decodeJson :: Either _ Meal of
  Left err -> show err
  Right m -> show m

----------
-- Main --
----------
main :: Effect Unit
main = launchAff_ do
  log meal
  processAjaxResult (Proxy :: _ GetPostRes) =<< Ajax.get ResponseFormat.string "https://jsonplaceholder.typicode.com/posts/1"
  processAjaxResult (Proxy :: _ GetPostResList) =<< Ajax.get ResponseFormat.string "https://jsonplaceholder.typicode.com/posts/"
  processAjaxResult (Proxy :: _ CreateBlogPostRes) =<< Ajax.post ResponseFormat.string "https://jsonplaceholder.typicode.com/posts" (Just $ RequestBody.String $ encodeJSON { title: "Title", body: "Post new blog entry", userId: 1 })

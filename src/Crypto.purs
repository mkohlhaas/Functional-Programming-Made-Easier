module Crypto where

import Prelude

import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.String (length)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Buffer (fromString, toString)
import Node.Crypto.Hash (createHash, update, digest)
import Node.Encoding (Encoding(Hex, UTF8))
import Random.LCG (Seed, mkSeed)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (sample)

type UserName = String
type Password = String

userNameSeed :: UserName -> Seed
userNameSeed userName = userName # toCharArray <#> toCharCode # foldl (*) 1 # mkSeed

userNameSalt :: Int -> UserName -> String
userNameSalt saltLength userName = fromCharArray $ sample (userNameSeed userName) saltLength arbitrary

passwordHashHex :: UserName -> Password -> Aff String
passwordHashHex userName password = do
  let salt = userNameSalt (3 * length userName) userName
  buf <- liftEffect $ fromString (password <> salt) UTF8
  liftEffect $ createHash "sha512" >>= update buf >>= digest >>= toString Hex

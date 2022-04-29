module Main where

import Prelude
import Effect (Effect)

import Ch25a as Ch25  -- encode/decode with Foreign
-- import Ch25b as Ch25  -- encode with Foreign, decode with Argonaut
-- import Ch25c as Ch25  -- encode with Foreign, decode with Argonaut in parallel

main :: Effect Unit
main = Ch25.test

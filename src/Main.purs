module Main where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, ($), (==))

------------------------------------------------------
-- Define Type Classes Semigroup, Monoid, and Group --
------------------------------------------------------

----------------
-- Data Types --
----------------

data AndBool = AFalse | ATrue
data OrBool = OFalse | OTrue
data Mod4 = Zero | One | Two | Three
newtype First a = First (Maybe a) -- prefer first Maybe with value
newtype Last a = Last (Maybe a) ---- prefer last Maybe with value

----------------------
-- Helper Functions --
----------------------

verifyAndBoolSemigroup ∷ Effect Unit
verifyAndBoolSemigroup = do
  log "Verifying AndBool Semigroup Laws"

verifyAndBoolMonoid ∷ Effect Unit
verifyAndBoolMonoid = do
  log "Verifying AndBool Monoid Laws"

verifyOrBoolSemigroup ∷ Effect Unit
verifyOrBoolSemigroup = do
  log "Verifying OrBool Semigroup Laws"

verifyOrBoolMonoid ∷ Effect Unit
verifyOrBoolMonoid = do
  log "Verifying OrBool Monoid Laws"

verifyMod4Semigroup ∷ Effect Unit
verifyMod4Semigroup = do
  log "Verifying Mod4 Semigroup Laws"

verifyMod4Monoid ∷ Effect Unit
verifyMod4Monoid = do
  log "Verifying Mod4 Monoid Laws"

verifyMod4Group ∷ Effect Unit
verifyMod4Group = do
  log "Verifying Mod4 Group Laws"

----------
-- Main --
----------

main ∷ Effect Unit
main = do
  log "Exercise Chapter 9."
  log $ show $ ATrue <> ATrue ----------------------------------------------- ATrue
  log $ show $ ATrue <> AFalse ---------------------------------------------- AFalse
  log $ show $ AFalse <> AFalse --------------------------------------------- AFalse
  log $ show $ AFalse <> mempty --------------------------------------------- AFalse
  log $ show $ ATrue <> mempty ---------------------------------------------- ATrue
  log $ show $ mempty <> ATrue == ATrue ------------------------------------- true
  log $ show $ mempty <> AFalse == ATrue ------------------------------------ false
  verifyAndBoolSemigroup
  verifyAndBoolMonoid
  verifyOrBoolSemigroup
  verifyOrBoolMonoid
  verifyMod4Semigroup
  verifyMod4Monoid
  verifyMod4Group
  log $ show $ First Nothing <> First (Just 77) ----------------------------- (First (Just 77))
  log $ show $ mempty <> First Nothing == First (Nothing ∷ Maybe Unit) ------ true
  log $ show $ mempty <> First (Just 77) == First (Just 77) ----------------- true
  log $ show $ Last (Just 1) <> Last (Just 99) ------------------------------ (Last (Just 99))
  log $ show $ mempty <> Last Nothing == Last (Nothing ∷ Maybe Unit) -------- true
  log $ show $ mempty <> Last (Just 77) == Last (Just 77) ------------------- true

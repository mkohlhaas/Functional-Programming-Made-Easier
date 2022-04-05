module Main where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, ($), (==))

---------------------------------------------------------
-- 1. Define Type Classes Semigroup, Monoid, and Group --
---------------------------------------------------------

class Semigroup a where
  append :: a -> a -> a

class Semigroup m <= Monoid m where
  mempty :: m

class Monoid g <= Group g where
  ginverse :: g -> g

----------------
-- Data Types --
----------------

data AndBool = AFalse | ATrue
data OrBool = OFalse | OTrue
data Mod4 = Zero | One | Two | Three
newtype First a = First (Maybe a) -- prefer first Maybe with value
newtype Last a = Last (Maybe a) -- prefer last Maybe with value

-------------------------------------------------------
---------------------- Instances ----------------------
-------------------------------------------------------

-------------
-- AndBool --
-------------
instance Semigroup AndBool where
  append ATrue ATrue = ATrue
  append _ _ = AFalse

instance Monoid AndBool where
  mempty = ATrue

derive instance Eq AndBool

derive instance Generic AndBool _

instance Show AndBool where
  show = genericShow

------------
-- OrBool --
------------
instance Semigroup OrBool where
  append OFalse OFalse = OFalse
  append _ _ = OTrue

instance Monoid OrBool where
  mempty = OFalse

derive instance Eq OrBool

derive instance Generic OrBool _

instance Show OrBool where
  show = genericShow

----------
-- Mod4 --
----------
derive instance Eq Mod4

instance Semigroup Mod4 where
  append Zero a = a
  append a Zero = a
  append One One = Two
  append One Two = Three
  append One Three = Zero
  append Two One = Three
  append Two Two = Zero
  append Two Three = One
  append Three One = Zero
  append Three Two = One
  append Three Three = Two

instance Monoid Mod4 where
  mempty = Zero

instance Group Mod4 where
  ginverse Zero = Zero
  ginverse One = Three
  ginverse Two = Two
  ginverse Three = One

-----------
-- First --
-----------
derive newtype instance Eq a => Eq (First a)
derive newtype instance Show a => Show (First a)

instance Semigroup (First a) where
  append first@(First (Just _)) _ = first
  append _ first = first

instance Monoid (First a) where
  mempty = First Nothing

----------
-- Last --
----------
derive newtype instance Eq a => Eq (Last a)
derive newtype instance Show a => Show (Last a)

instance Semigroup (Last a) where
  append last@(Last (Just _)) (Last Nothing) = last
  append _ last = last

instance Monoid (Last a) where
  mempty = Last Nothing

---------------
-- Operators --
---------------

infixr 5 append as <>

----------------------
-- Helper Functions --
----------------------

verifyAndBoolSemigroup :: Effect Unit
verifyAndBoolSemigroup = do
  log "Verifying AndBool Semigroup Laws"
  log $ show $ (AFalse <> AFalse) <> AFalse == AFalse <> (AFalse <> AFalse)
  log $ show $ (AFalse <> AFalse) <> ATrue == AFalse <> (AFalse <> ATrue)
  log $ show $ (AFalse <> ATrue) <> AFalse == AFalse <> (ATrue <> AFalse)
  log $ show $ (AFalse <> ATrue) <> ATrue == AFalse <> (ATrue <> ATrue)
  log $ show $ (ATrue <> AFalse) <> AFalse == ATrue <> (AFalse <> AFalse)
  log $ show $ (ATrue <> AFalse) <> ATrue == ATrue <> (AFalse <> ATrue)
  log $ show $ (ATrue <> ATrue) <> AFalse == ATrue <> (ATrue <> AFalse)
  log $ show $ (ATrue <> ATrue) <> ATrue == ATrue <> (ATrue <> ATrue)

verifyAndBoolMonoid :: Effect Unit
verifyAndBoolMonoid = do
  log "Verifying AndBool Monoid Laws"
  log $ show $ (mempty <> AFalse) == AFalse
  log $ show $ (AFalse <> mempty) == AFalse
  log $ show $ (mempty <> ATrue) == ATrue
  log $ show $ (ATrue <> mempty) == ATrue

verifyOrBoolSemigroup :: Effect Unit
verifyOrBoolSemigroup = do
  log "Verifying OrBool Semigroup Laws"
  log $ show $ (OFalse <> OFalse) <> OFalse == OFalse <> (OFalse <> OFalse)
  log $ show $ (OFalse <> OFalse) <> OTrue == OFalse <> (OFalse <> OTrue)
  log $ show $ (OFalse <> OTrue) <> OFalse == OFalse <> (OTrue <> OFalse)
  log $ show $ (OFalse <> OTrue) <> OTrue == OFalse <> (OTrue <> OTrue)
  log $ show $ (OTrue <> OFalse) <> OFalse == OTrue <> (OFalse <> OFalse)
  log $ show $ (OTrue <> OFalse) <> OTrue == OTrue <> (OFalse <> OTrue)
  log $ show $ (OTrue <> OTrue) <> OFalse == OTrue <> (OTrue <> OFalse)
  log $ show $ (OTrue <> OTrue) <> OTrue == OTrue <> (OTrue <> OTrue)

verifyOrBoolMonoid :: Effect Unit
verifyOrBoolMonoid = do
  log "Verifying OrBool Monoid Laws"
  log $ show $ (mempty <> OFalse) == OFalse
  log $ show $ (OFalse <> mempty) == OFalse
  log $ show $ (mempty <> OTrue) == OTrue
  log $ show $ (OTrue <> mempty) == OTrue

verifyMod4Semigroup :: Effect Unit
verifyMod4Semigroup = do
  log "Verifying Mod4 Semigroup Laws"
  log $ show $ (Zero <> Zero) <> Zero == Zero <> (Zero <> Zero)
  log $ show $ (Zero <> Zero) <> One == Zero <> (Zero <> One)
  log $ show $ (Zero <> Zero) <> Two == Zero <> (Zero <> Two)
  log $ show $ (Zero <> Zero) <> Three == Zero <> (Zero <> Three)
  log $ show $ (Zero <> One) <> Zero == Zero <> (One <> Zero)
  log $ show $ (Zero <> One) <> One == Zero <> (One <> One)
  log $ show $ (Zero <> One) <> Two == Zero <> (One <> Two)
  log $ show $ (Zero <> One) <> Three == Zero <> (One <> Three)
  log $ show $ (Zero <> Two) <> Zero == Zero <> (Two <> Zero)
  log $ show $ (Zero <> Two) <> One == Zero <> (Two <> One)
  log $ show $ (Zero <> Two) <> Two == Zero <> (Two <> Two)
  log $ show $ (Zero <> Two) <> Three == Zero <> (Two <> Three)
  log $ show $ (Zero <> Three) <> Zero == Zero <> (Three <> Zero)
  log $ show $ (Zero <> Three) <> One == Zero <> (Three <> One)
  log $ show $ (Zero <> Three) <> Two == Zero <> (Three <> Two)
  log $ show $ (Zero <> Three) <> Three == Zero <> (Three <> Three)
  log $ show $ (One <> Zero) <> Zero == One <> (Zero <> Zero)
  log $ show $ (One <> Zero) <> One == One <> (Zero <> One)
  log $ show $ (One <> Zero) <> Two == One <> (Zero <> Two)
  log $ show $ (One <> Zero) <> Three == One <> (Zero <> Three)
  log $ show $ (One <> One) <> Zero == One <> (One <> Zero)
  log $ show $ (One <> One) <> One == One <> (One <> One)
  log $ show $ (One <> One) <> Two == One <> (One <> Two)
  log $ show $ (One <> One) <> Three == One <> (One <> Three)
  log $ show $ (One <> Two) <> Zero == One <> (Two <> Zero)
  log $ show $ (One <> Two) <> One == One <> (Two <> One)
  log $ show $ (One <> Two) <> Two == One <> (Two <> Two)
  log $ show $ (One <> Two) <> Three == One <> (Two <> Three)
  log $ show $ (One <> Three) <> Zero == One <> (Three <> Zero)
  log $ show $ (One <> Three) <> One == One <> (Three <> One)
  log $ show $ (One <> Three) <> Two == One <> (Three <> Two)
  log $ show $ (One <> Three) <> Three == One <> (Three <> Three)
  log $ show $ (Two <> Zero) <> Zero == Two <> (Zero <> Zero)
  log $ show $ (Two <> Zero) <> One == Two <> (Zero <> One)
  log $ show $ (Two <> Zero) <> Two == Two <> (Zero <> Two)
  log $ show $ (Two <> Zero) <> Three == Two <> (Zero <> Three)
  log $ show $ (Two <> One) <> Zero == Two <> (One <> Zero)
  log $ show $ (Two <> One) <> One == Two <> (One <> One)
  log $ show $ (Two <> One) <> Two == Two <> (One <> Two)
  log $ show $ (Two <> One) <> Three == Two <> (One <> Three)
  log $ show $ (Two <> Two) <> Zero == Two <> (Two <> Zero)
  log $ show $ (Two <> Two) <> One == Two <> (Two <> One)
  log $ show $ (Two <> Two) <> Two == Two <> (Two <> Two)
  log $ show $ (Two <> Two) <> Three == Two <> (Two <> Three)
  log $ show $ (Two <> Three) <> Zero == Two <> (Three <> Zero)
  log $ show $ (Two <> Three) <> One == Two <> (Three <> One)
  log $ show $ (Two <> Three) <> Two == Two <> (Three <> Two)
  log $ show $ (Two <> Three) <> Three == Two <> (Three <> Three)
  log $ show $ (Three <> Zero) <> Zero == Three <> (Zero <> Zero)
  log $ show $ (Three <> Zero) <> One == Three <> (Zero <> One)
  log $ show $ (Three <> Zero) <> Two == Three <> (Zero <> Two)
  log $ show $ (Three <> Zero) <> Three == Three <> (Zero <> Three)
  log $ show $ (Three <> One) <> Zero == Three <> (One <> Zero)
  log $ show $ (Three <> One) <> One == Three <> (One <> One)
  log $ show $ (Three <> One) <> Two == Three <> (One <> Two)
  log $ show $ (Three <> One) <> Three == Three <> (One <> Three)
  log $ show $ (Three <> Two) <> Zero == Three <> (Two <> Zero)
  log $ show $ (Three <> Two) <> One == Three <> (Two <> One)
  log $ show $ (Three <> Two) <> Two == Three <> (Two <> Two)
  log $ show $ (Three <> Two) <> Three == Three <> (Two <> Three)
  log $ show $ (Three <> Three) <> Zero == Three <> (Three <> Zero)
  log $ show $ (Three <> Three) <> One == Three <> (Three <> One)
  log $ show $ (Three <> Three) <> Two == Three <> (Three <> Two)
  log $ show $ (Three <> Three) <> Three == Three <> (Three <> Three)

verifyMod4Monoid :: Effect Unit
verifyMod4Monoid = do
  log "Verifying Mod4 Monoid Laws"
  log $ show $ Zero <> mempty == Zero
  log $ show $ One <> mempty == One
  log $ show $ Two <> mempty == Two
  log $ show $ Three <> mempty == Three
  log $ show $ mempty <> Zero == Zero
  log $ show $ mempty <> One == One
  log $ show $ mempty <> Two == Two
  log $ show $ mempty <> Three == Three

verifyMod4Group :: Effect Unit
verifyMod4Group = do
  log "Verifying Mod4 Group Laws"
  log $ show $  ginverse Zero <> Zero == mempty
  log $ show $  ginverse One <> One == mempty
  log $ show $  ginverse Two <> Two == mempty
  log $ show $  ginverse Three <> Three == mempty
  log $ show $  Zero <> ginverse Zero == mempty
  log $ show $  One <> ginverse One == mempty
  log $ show $  Two <> ginverse Two == mempty
  log $ show $  Three <> ginverse Three == mempty

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Exercise Chapter 9."
  log $ show $ ATrue  <> ATrue                                             -- ATrue
  log $ show $ ATrue  <> AFalse                                            -- AFalse
  log $ show $ AFalse <> AFalse                                            -- AFalse
  log $ show $ AFalse <> mempty                                            -- AFalse
  log $ show $ ATrue  <> mempty                                            -- ATrue
  log $ show $ mempty <> ATrue  == ATrue                                   -- true
  log $ show $ mempty <> AFalse == ATrue                                   -- false
  verifyAndBoolSemigroup
  verifyAndBoolMonoid
  verifyOrBoolSemigroup
  verifyOrBoolMonoid
  verifyMod4Semigroup
  verifyMod4Monoid
  verifyMod4Group
  log $ show $ First Nothing <> First (Just 77)                            -- (First (Just 77)) or (Just 77)
  log $ show $ mempty <> First Nothing   == First (Nothing :: Maybe Unit)  -- true
  log $ show $ mempty <> First (Just 77) == First (Just 77)                -- true
  log $ show $ Last (Just 1) <> Last (Just 99)                             -- (Last (Just 99)) or (Just 99)
  log $ show $ mempty <> Last Nothing   == Last (Nothing :: Maybe Unit)    -- true
  log $ show $ mempty <> Last (Just 77) == Last (Just 77)                  -- true

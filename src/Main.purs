module Main where

import Prelude hiding (ap, (*>))

import Data.Int.Bits ((.&.))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)

oddTest ∷ Int → Maybe Int
oddTest x = if x .&. 1 == 1 then Just x else Nothing

greaterThanTest ∷ Int → Int → Maybe Int
greaterThanTest min x = if x > min then Just x else Nothing

lessThanTest ∷ Int → Int → Maybe Int
lessThanTest max x = if x < max then Just x else Nothing

-- Rewrite the following function in do-notation. (Comment out old function version.)
-- gauntlet ∷ Int → Maybe Int
-- gauntlet x =
--   pure x >>= oddTest
--     >>= \o → pure (o + 1)
--       >>= \y → greaterThanTest 10 y
--         >>= \z → lessThanTest 20 z

-- gauntlet ∷ Int → Maybe Int
-- gauntlet x = do
--   o ← oddTest x
--   let y = o + 1
--   void $ greaterThanTest 10 y
--   lessThanTest 20 y

------------------------------
-- applyFirst & applySecond --
------------------------------

-- Combine two effectful actions, keeping only the result of the first.
applyFirst ∷ ∀ a b f. Apply f ⇒ f a → f b → f a
applyFirst a b = const <$> a <*> b

-- Uncomment the following line.
infixl 4 applyFirst as <*

-- Combine two effectful actions, keeping only the result of the second.
applySecond ∷ ∀ a b f. Apply f ⇒ f a → f b → f b
applySecond a b = const identity <$> a <*> b

-- Uncomment the following line.
infixl 4 applySecond as *>

-- Rewrite the rewritten function gauntlet in do-notation with bind (>>=) again.
gauntlet ∷ Int → Maybe Int
gauntlet x = oddTest x >>= \o → pure (o + 1) >>= \y → greaterThanTest 10 y *> lessThanTest 20 y

-- Provide a default implementation of apply (<*>) for any Monad using 'bind'.
-- Writing apply in terms of bind.
ap ∷ ∀ m a b. Monad m ⇒ m (a → b) → m a → m b
ap f a = do
  f' ← f
  a' ← a
  pure $ f' a'

------------------
-- Writer Monad --
------------------

-- Define the data definition for the Writer monad.
newtype Writer w a = Writer (Tuple a w)

-- Implement the Writer monad.
instance Monoid w ⇒ Applicative (Writer w) where
  pure x = Writer (Tuple x mempty)

-- instance Semigroup w ⇒ Apply (Writer w) where
--   apply (Writer (Tuple f w)) (Writer (Tuple x w')) = Writer (Tuple (f x) (w <> w'))

instance Semigroup w ⇒ Functor (Writer w) where
  map f (Writer (Tuple x w)) = Writer (Tuple (f x) w)

instance Monoid w ⇒ Bind (Writer w) where
  bind (Writer (Tuple x _)) f = f x

instance Monoid w ⇒ Monad (Writer w)

-- In a second version use 'ap' for implementing Apply. (Comment out previous version.)
instance Monoid w ⇒ Apply (Writer w) where
  apply = ap

--------------------------------------------------------------------------------------
-- Implement Helper Functions for the Writer Monad (Writer API): tell, listen, pass --
--------------------------------------------------------------------------------------

-- append a value to the accumulator/log
tell ∷ ∀ w. w → Writer w Unit
tell w = Writer (Tuple unit w)

-- modify the result to include the changes to the accumulator/log
listen ∷ ∀ a w. Writer w a → Writer w (Tuple a w)
listen (Writer (Tuple x w)) = Writer (Tuple (Tuple x w) w)

-- apply the returned function to the accumulator/log
pass ∷ ∀ a w. Writer w (Tuple a (w → w)) → Writer w a
pass (Writer (Tuple (Tuple x f) w)) = Writer (Tuple x (f w))

------------------
-- Reader Monad --
------------------

-- Define the data definition for the Reader monad.
newtype Reader r a = Reader (r → a)

-- Implement runReader.
runReader ∷ ∀ r a. Reader r a → r → a
runReader (Reader f) = f

-- Implement the Reader monad.
-- Function Application, $, gets replaced with Function Composition, <<<, during an Eta-reduction step.
instance Applicative (Reader r) where
  pure = Reader <<< const

instance Apply (Reader r) where
  apply (Reader f) (Reader x) = Reader \r → f r $ x r

instance Functor (Reader r) where
  map f (Reader x) = Reader \r → f $ x r

instance Bind (Reader r) where
  bind (Reader x) f = Reader \r → f (x r) # \(Reader y) → y r

instance Monad (Reader r)

-----------------------------------------------------------------------------
-- Implement Helper Functions for the Reader Monad (Reader API): ask, asks --
-----------------------------------------------------------------------------

-- get the global context/configuration
ask ∷ ∀ r. Reader r r
ask = Reader identity

-- project a value from the global context/configuration
asks ∷ ∀ a r. (r → a) → Reader r a
asks f = Reader \r → f r

-----------------
-- State Monad --
-----------------

-- Define the data definition for the State monad.
newtype State s a = State (s → (Tuple a s))

-- Implement runState.
runState ∷ ∀ s a. State s a → s → Tuple a s
runState (State f) s = f s

-------------------------------
-- Implement the State Monad --
-------------------------------

-- Implement the helper function 'state'.
state ∷ ∀ s a. (s → Tuple a s) → State s a
state f = State \s → f s

-------------------------------------------------------------------------------------------------
-- Implement Helper Functions for the State Monad (State API): get, gets, put, modify, modify_ --
-------------------------------------------------------------------------------------------------

-- get the current state
get ∷ ∀ s. State s s
get = state \s → Tuple s s

-- get a value which depends on the current state
gets ∷ ∀ s a. (s → a) → State s a
gets f = state \s → Tuple (f s) s

-- set the state
put ∷ ∀ s. s → State s Unit
put s = state $ const (Tuple unit s)

-- Modify the state by applying a function to the current state. The returned value is the new state value.
modify ∷ ∀ s. (s → s) → State s s
modify f = state \s → Tuple (f s) s

-- A version of modify which does not return the updated value.
-- In practice, modify_ is used more often since it doesn't require a void in a do block and we rarely need the new State back.
-- But, when we do, modify will save us a call to get.
modify_ ∷ ∀ s. (s → s) → State s Unit
modify_ f = state \s → f s # const (Tuple unit s)

main ∷ Effect Unit
main = do
  logShow $ gauntlet 5
  logShow $ gauntlet 10
  logShow $ gauntlet 11

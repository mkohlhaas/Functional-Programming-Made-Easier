module Main where

import Prelude

import Data.Int.Bits ((.&.))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)

oddTest ∷ Int → Maybe Int
oddTest x = if x .&. 1 == 1 then Just x else Nothing

greaterThanTest ∷ Int → Int → Maybe Int
greaterThanTest min x = if x > min then Just x else Nothing

lessThanTest ∷ Int → Int → Maybe Int
lessThanTest max x = if x < max then Just x else Nothing

-- Rewrite the following function in do-notation.
gauntlet ∷ Int → Maybe Int
gauntlet x =
  pure x >>= oddTest
    >>= \o → pure (o + 1)
      >>= \y → greaterThanTest 10 y
        >>= \z → lessThanTest 20 z

----------------------------
-- applyFirst applySecond --
----------------------------

-- Combine two effectful actions, keeping only the result of the first.
-- applyFirst ∷ ∀ a b f. Apply f ⇒ f a → f b → f a

-- Uncomment the following line.
-- infixl 4 applyFirst as <*

-- Combine two effectful actions, keeping only the result of the second.
-- applySecond ∷ ∀ a b f. Apply f ⇒ f a → f b → f b

-- Uncomment the following line.
-- infixl 4 applySecond as *>

-- Rewrite the rewritten function gauntlet in do-notation with bind (>>=) again.

-- Provide a default implementation of apply (<*>) for any Monad using 'bind'.
-- ap ∷ ∀ m a b. Monad m ⇒ m (a → b) → m a → m b

------------------
-- Writer Monad --
------------------

-- Define the data definition for the Writer monad.

-- Implement the Writer monad.

-- In a second version use 'ap' for implementing Apply.

-- Implement helper functions for the Writer monad(Writer API): tell, listen, pass.

-- append a value to the accumulator/log
-- tell ∷ ∀ w. w → Writer w Unit

-- modify the result to include the changes to the accumulator/log
-- listen ∷ ∀ a w. Writer w a → Writer w (Tuple a w)

-- apply the returned function to the accumulator/log
-- pass ∷ ∀ a w. Writer w (Tuple a (w → w)) → Writer w a

------------------
-- Reader Monad --
------------------

-- Define the data definition for the Reader monad.

-- Implement runReader.
-- runReader ∷ ∀ r a. Reader r a → r → a

-- Implement the Reader monad.

-- Implement helper functions for the Reader monad(Reader API): ask, asks.

-- get the global context/configuration
-- ask ∷ ∀ r. Reader r r

-- project a value from the global context/configuration
-- asks ∷ ∀ a r. (r → a) → Reader r a

-----------------
-- State Monad --
-----------------

-- Define the data definition for the State monad.

-- Implement runState.
-- runState ∷ ∀ s a. State s a → s → Tuple a s

-- Implement the State monad.

-- Implement the helper function 'state'.
-- state ∷ ∀ a. (s → (Tuple a s)) → m a

-- Implement helper functions for the State monad(State API): get, gets, put, modify, modify_.

-- get the current state
-- get ∷ ∀ s. State s s

-- get a value which depends on the current state
-- gets ∷ ∀ s a. (s → a) → State s a

-- set the state
-- put ∷ ∀ s. s → State s Unit

-- Modify the state by applying a function to the current state. The returned value is the new state value.
-- modify ∷ ∀ s. (s → s) → State s s

-- A version of modify which does not return the updated value.
-- modify_ ∷ ∀ s. (s → s) → State s Unit

main ∷ Effect Unit
main = do
  logShow $ gauntlet 5
  logShow $ gauntlet 10
  logShow $ gauntlet 11

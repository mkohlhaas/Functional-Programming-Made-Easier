module Main where

import Prelude

import Control.Monad.Except.Trans (class MonadError, class MonadThrow, ExceptT, catchError, runExceptT, throwError)
import Control.Monad.Reader.Trans (class MonadAsk, ask)
import Control.Monad.State.Trans (class MonadState, get, put)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Trans (class MonadTell, WriterT, runWriterT, tell)
import Data.Either (Either)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

-- 1. Write StateT type definition.

-- 2. Write runStateT.

-- 3. Write Functor instance.

-- 4. Write a Functor instance with only a Functor constraint.

-- 5. Write Apply instance.

-- 6. Write Applicative instance.

-- 7. Write Bind instance.

-- 8. Write Monad instance.

-- 9. Write MonadState instance.

-- 10. Write MonadTrans instance.

-- 11. Write MonadAsk instance.

-- 12. Write MonadTell instance.

-- 13. Write monadAsk and monadTell in terms of MonadTrans, i.e. with lift.

-- 14. Write MonadThrow instance.

-- 15. Write MonadError instance.

-------------
-- Testing --
-------------

-- This is our Monad stack:

type AppStack e w s a = ExceptT e (WriterT w (StateT s Effect)) a

-- 16. Write runApp to run AppStack.
-- Hint: Use a type hole to find out the result type.

-- 17. Factor out the return type, call it StackResult and update runApp.

----------------------
-- Helper functions --
----------------------

logM ∷ ∀ m. MonadTell (Array String) m ⇒ String → m Unit
logM s = tell [ s <> "\n" ]

validate ∷ Int → AppM
validate n = when (n == 0) $ void $ throwError "WE CANNOT HAVE A 0 STATE!"

-- our specific monad stack
type AppM = AppStack String (Array String) Int Unit

app ∷ AppM
app = do
  logM "Starting App..."
  n ← get
  when (n == 0) $ void $ throwError "WE CANNOT HAVE A ZERO STATE!"
  put $ n + 1
  logM "Incremented State"
  pure unit

app2 ∷ AppM
app2 = do
  logM "Starting App..."
  n ← get
  catchError (validate n)
    ( \err → do
        tell [ "We encountered an error: " <> err ]
        put 1
    )
  put $ n + 1
  logM "Incremented State"
  pure unit

----------
-- Main --
----------

main ∷ Effect Unit
main = do
  log "Exercise Chapter 21."
  result1 ← runApp 0 app1
  log $ show $ result1 == (Tuple (Tuple (Left "WE CANNOT HAVE A ZERO STATE!") [ "Starting App...\n" ]) 0)
  result2 ← runApp 99 app1
  log $ show $ result2 == (Tuple (Tuple (Right unit) [ "Starting App...\n", "Incremented State\n" ]) 100)
  result3 ← runApp 0 app2
  log $ show $ result3 == (Tuple (Tuple (Right unit) [ "Starting App...\n", "We encountered an error: WE CANNOT HAVE A 0 STATE!", "Incremented State\n" ]) 1)
  result4 ← runApp 99 app2
  log $ show $ result4 == (Tuple (Tuple (Right unit) [ "Starting App...\n", "Incremented State\n" ]) 100)

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
-- Note: Only Functor constraint is allowed!

-- 4. Write Apply instance.

-- 5. Write Applicative instance.

-- 6. Write Bind instance.

-- 7. Write Monad instance.

-- 8. Write MonadState instance.

-- 9. Write MonadTrans instance.

-- 10. Write MonadAsk instance.

-- 11. Write MonadTell instance.

-- 12. Write monadAskStateT and monadTellStateT in terms of MonadTrans, i.e. with lift.

-- 13. Write MonadThrow instance.

-- 14. Write MonadError instance.

-------------
-- Testing --
-------------

-- This is our Monad stack:
type AppStack e w s a = ExceptT e (WriterT w (StateT s Effect)) a

-- 15. Write runApp to run AppStack.

-- 16. Factor out the return type, call it StackResult and update runApp.

-- Given helper function.
logM ∷ ∀ m. MonadTell (Array String) m ⇒ String → m Unit
logM s = tell [ s <> "\n" ]

-- This is our specific monad stack:
type AppM = AppStack String (Array String) Int Unit

app ∷ AppM
app = do
  logM "Starting App..."
  n ← get
  when (n == 0) $ void $ throwError "WE CANNOT HAVE A ZERO STATE!"
  put $ n + 1
  logM "Incremented State"
  pure unit

----------
-- Main --
----------

main ∷ Effect Unit
main = do
  log "Exercise Chapter 21."
  result1 ← runApp 0 app
  log $ show $ result1 == (Tuple (Tuple (Left "WE CANNOT HAVE A ZERO STATE!") ["Starting App...\n"]) 0)
  result2 ← runApp 99 app
  log $ show $ result2 == (Tuple (Tuple (Right unit) ["Starting App...\n","Incremented State\n"]) 100)

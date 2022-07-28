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
newtype StateT s m a = StateT (s → m (Tuple a s))

-- 2. Write runStateT.
runStateT ∷ ∀ s m a. StateT s m a -> s -> m (Tuple a s)
runStateT (StateT f) = f

-- 3. Write Functor instance.
-- Note: Only Functor constraint is allowed!
instance Functor m ⇒ Functor (StateT s m) where
  map f x = StateT \s → runStateT x s <#> \(Tuple x' s') → Tuple (f x') s'

-- Using bind for inner monad.
-- instance Monad m ⇒ Functor (StateT s m) where
--   map f x = StateT \s → runStateT x s >>= \(Tuple x' s') → pure $ Tuple (f x') s'

-- 4. Write Apply instance.
instance Monad m ⇒ Apply (StateT s m) where
  apply = ap

-- Using bind for inner monad.
-- instance Monad m ⇒ Apply (StateT s m) where
--   apply f x = StateT \s → runStateT f s >>= \(Tuple f' s') → runStateT x s' >>= \(Tuple x' s'') → pure $ Tuple (f' x') s''

-- Using bind with do-notation (same as `ap`).
-- instance Monad m ⇒ Apply (StateT s m) where
--   apply f x = StateT \s → do
--     Tuple f' s' ← runStateT f s
--     Tuple x' s'' ← runStateT x s'
--     pure $ Tuple (f' x') s''

-- 5. Write Applicative instance.
instance Monad m ⇒ Applicative (StateT s m) where
  pure x = StateT \s → pure $ Tuple x s

-- 6. Write Bind instance.
-- Using inner monad's bind.
instance Monad m ⇒ Bind (StateT s m) where
  bind x f = StateT \s → runStateT x s >>= \(Tuple x' s') → runStateT (f x') s'

-- 7. Write Monad instance.
instance Monad m ⇒ Monad (StateT s m)

-- 8. Write MonadState instance.
instance Monad m ⇒ MonadState s (StateT s m) where
  state f = StateT $ pure <<< f

-- 9. Write MonadTrans instance.
instance MonadTrans (StateT s) where
  lift x = StateT \s → x <#> \x' → Tuple x' s

-- Using inner monad's bind.
-- instance MonadTrans (StateT s) where
--   lift x = StateT \s → x >>= \x' → pure $ Tuple x' s

-- 10. Write MonadAsk instance.
-- instance MonadAsk r m ⇒ MonadAsk r (StateT s m) where
--   ask = StateT \s → ask <#> \r → Tuple r s

-- Using inner monad's bind.
-- instance MonadAsk r m ⇒ MonadAsk r (StateT s m) where
--   ask = StateT \s → do
--     r ← ask
--     pure $ Tuple r s

-- 11. Write MonadTell instance.
-- instance MonadTell w m ⇒ MonadTell w (StateT s m) where
--   tell w = StateT \s → tell w <#> \_ → Tuple unit s

-- Using inner monad's bind.
-- instance MonadTell w m ⇒ MonadTell w (StateT s m) where
--   tell w = StateT \s → do
--     tell w
--     pure $ Tuple unit s

-- 12. Write monadAskStateT and monadTellStateT in terms of MonadTrans, i.e. with lift.
instance MonadAsk r m ⇒ MonadAsk r (StateT s m) where
  ask = lift ask

instance MonadTell w m ⇒ MonadTell w (StateT s m) where
  tell = lift <<< tell

-- 13. Write MonadThrow instance.
instance MonadThrow e m ⇒ MonadThrow e (StateT s m) where
  throwError = lift <<< throwError

-- 14. Write MonadError instance.
instance MonadError e m ⇒ MonadError e (StateT s m) where
  catchError x f = StateT \s → catchError (runStateT x s) \e → runStateT (f e) s

-------------
-- Testing --
-------------

-- This is our monad stack:
type AppStack e w s a = ExceptT e (WriterT w (StateT s Effect)) a

-- 15. Write runApp to run AppStack.
runApp ∷ ∀ s e w a. s → AppStack e w s a → Effect (StackResult e w s a)
runApp s = runExceptT >>> runWriterT >>> flip runStateT s

-- 16. Factor out the return type, call it StackResult and update runApp.
type StackResult e w s a = (Tuple (Tuple (Either e a) w) s)

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
  -- We could transform the output to something nicer, but ... we call it a day!
  log $ show result1 ------------- (Tuple (Tuple (Left "WE CANNOT HAVE A ZERO STATE!") ["Starting App...\n"]) 0)
  result2 ← runApp 99 app
  log $ show result2 ------------- (Tuple (Tuple (Right unit) ["Starting App...\n","Incremented State\n"]) 100)

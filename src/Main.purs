module Main where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.State.Class (class MonadState, get, put)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

-- 1. Write StateT type definition.
newtype StateT s m a = StateT (s -> m (Tuple a s))

-- 2. Write runStateT.
runStateT :: ∀ s m a. StateT s m a -> s -> m (Tuple a s)
runStateT (StateT f) = f

-- 3. Write Functor instance.
-- Note: m should be a Functor not a Monad to minimize constraints!
instance Functor m => Functor (StateT s m) where
  map f g = StateT \s -> runStateT g s <#> \(Tuple a s') -> Tuple (f a) s'

-- instance Monad m => Functor (StateT s m) where
--   map f g = StateT \s -> runStateT g s >>= \(Tuple a s') -> pure $ Tuple (f a) s'

-- 4. Write Apply instance.

instance Monad m => Apply (StateT s m) where
  apply f x = StateT \s -> do
    Tuple f' s' <- runStateT f s
    Tuple a s'' <- runStateT x s'
    pure $ Tuple (f' a) s''

-- instance Monad m => Apply (StateT s m) where
--   apply f x = StateT \s -> runStateT f s >>= \(Tuple f' s') -> runStateT x s' >>= \(Tuple a s'') -> pure $ Tuple (f' a) s''

-- instance Monad m => Apply (StateT s m) where
--   apply = ap

-- 5. Write Applicative instance.
instance Monad m => Applicative (StateT s m) where
  pure x = StateT \s -> pure $ Tuple x s

-- 6. Write Bind instance.
instance Monad m => Bind (StateT s m) where
  bind x f = StateT \s -> runStateT x s >>= \(Tuple a s') -> runStateT (f a) s'

-- 7. Write Monad instance.
instance Monad m => Monad (StateT s m)

-- 8. Write MonadState instance.
instance Monad m => MonadState s (StateT s m) where
  state f = StateT \s -> pure $ f s

-- 9. Write MonadTrans instance.
instance MonadTrans (StateT s) where
  lift ma = StateT \s -> ma >>= \a -> pure $ Tuple a s

-- 10. Write MonadAsk instance.
-- instance MonadAsk r m => MonadAsk r (StateT s m) where
--   ask = StateT \s -> ask <#> \r -> Tuple r s

-- 10. Write MonadTell instance.
-- instance MonadTell w m => MonadTell w (StateT s m) where
--   tell w = StateT \s -> tell w <#> \_ -> Tuple unit s

-- 11. Write monadAskStateT and monadTellStateT in terms of MonadTrans, i.e. with lift.
instance MonadAsk r m => MonadAsk r (StateT s m) where
  ask = lift ask

instance MonadTell w m => MonadTell w (StateT s m) where
  tell = lift <<< tell

-- 12. Write MonadThrow instance.
instance MonadThrow e m => MonadThrow e (StateT s m) where
  throwError = lift <<< throwError

-- 13. Write MonadError instance.
instance MonadError e m => MonadError e (StateT s m) where
  catchError (StateT fmx) f = StateT \s -> catchError (fmx s) \e -> runStateT (f e) s

-- This is our Monad stack:
type AppStack e w s a = ExceptT e (WriterT w (StateT s Effect)) a
type AppM = AppStack String String Int Unit

-- 13. Write runApp to run AppM.
-- runApp :: Int -> AppM -> Effect StackResult
-- runApp st = flip runStateT st <<< runWriterT <<< runExceptT

-- 16. Factor out the return type, call it StackResult and update runApp.
-- type StackResult = ...
type StackResult = Tuple (Tuple (Either String Unit) String) Int

-- AppEffects stores all side-effects of the monad stack.
type AppEffects = { log :: String, state :: Int, result :: Maybe Unit }

-- AppResult contains our side-effect values from running our Monad Stack AppEffects and, optionally, the error if one occurred.
type AppResult = Tuple (Maybe String) AppEffects

-- 16. Write a mapping function "results" that turns StackResult into an AppResult. Change runApp using "results".
-- results :: StackResult -> AppResult
results :: StackResult -> AppResult
results (Tuple (Tuple (Left err) l) s) = Tuple (Just err) { log: l, state: s, result: Nothing }
results (Tuple (Tuple (Right result) l) s) = Tuple Nothing { log: l, state: s, result: Just result }

runApp :: Int -> AppM -> Effect AppResult
runApp st = (results <$> _) <<< flip runStateT st <<< runWriterT <<< runExceptT

-- 17. Write helper function "logM" that appends a newline after every string in the writer.
logM :: ∀ m. MonadTell String m => String -> m Unit
logM s = tell $ s <> "\n"

-- 18. Write the application monad.
-- For logging use logM.

-- app :: AppM

-- The body of "app" contains the following actions in the specified order:
-- a) Write to the log "Starting App...".
-- b) Get the State.
-- c) Check the State to make sure it is non-zero, otherwise error with "WE CANNOT HAVE 0 STATE!".
-- d) Add 1 to the State.
-- e) Write to the log "Incremented State".
-- f) Return the Pure Computational Value.

app :: AppM
app = do
  logM "Starting App..."
  n <- get
  when (n == 0) $ void $ throwError "WE CANNOT HAVE A ZERO STATE!"
  put $ n + 1
  logM "Incremented State"
  pure unit

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Exercise Chapter 21."
  result1 <- runApp 0 app
  log $ show result1 -- (Tuple (Just "WE CANNOT HAVE A ZERO STATE!") { log: "Starting App...\n", result: Nothing, state: 0 })
  result2 <- runApp 99 app
  log $ show result2 -- (Tuple Nothing { log: "Starting App...\nIncremented State\n", result: (Just unit), state: 100 })

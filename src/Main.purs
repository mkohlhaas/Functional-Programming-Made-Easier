module Main where
import Prelude

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Writer.Trans (WriterT)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Console (log)

-------------------------------------
-- 1. Write StateT type definition --
-------------------------------------

newtype StateT s m a = StateT (s -> m (Tuple a s))

------------------------
-- 2. Write runStateT --
------------------------

-- runStateT :: ...

----------------------
-- 3. Write Functor --
----------------------

-- class Functor f where
-- map :: forall a b. (a -> b) -> f a -> f b

--------------------
-- 4. Write Apply --
--------------------

-- class (Functor f) <= Apply f where
-- apply :: forall a b. f (a -> b) -> f a -> f b

--------------------------
-- 5. Write Applicative --
--------------------------

-- class (Apply f) <= Applicative f where
-- pure :: forall a. a -> f a

-------------------
-- 6. Write Bind --
-------------------

-- class (Apply m) <= Bind m where
-- bind :: forall a b. m a -> (a -> m b) -> m b

--------------------
-- 7. Write Monad --
--------------------

-- class (Applicative m, Bind m) <= Monad m

-------------------------
-- 8. Write MonadState --
-------------------------

-- class (Monad m) <= MonadState s m | m -> s where
-- state :: forall a. (s -> (Tuple a s)) -> m a

-----------------------
-- 9. Write MonadAsk --
-----------------------

-- class (Monad m) <= MonadAsk r m | m -> r where
-- ask :: m r

-- instance monadAskStateT :: MonadAsk r m => MonadAsk r (StateT s m) where
--   ask = StateT \s -> ask <#> \r -> Tuple r s

------------------------
-- 10. Write MonadTell --
------------------------

-- class (Semigroup w, Monad m) <= MonadTell w m | m -> w where
-- tell :: w -> m Unit

-- instance monadTellStateT :: MonadTell w m => MonadTell w (StateT s m) where
--   tell w = StateT \s -> tell w <#> \_ -> Tuple unit s
-- tell w = StateT \s -> tell w <#> \unit -> Tuple unit s

--------------------------
-- 11. Write MonadTrans --
--------------------------

-- class MonadTrans t where
-- lift :: forall m a. Monad m => m a -> t m a

-------------------------------------------------------------------------
-- 12. Write monadAskStateT and monadTellStateT in terms of MonadTrans --
-------------------------------------------------------------------------

--------------------
-- 13. MonadThrow --
--------------------

-- class (Monad m) <= MonadThrow e m | m -> e where
-- throwError :: forall a. e -> m a

--------------------
-- 14. MonadError --
--------------------

-- class (MonadThrow e m) <= MonadError e m | m -> e where
-- catchError :: forall a. m a -> (e -> m a) -> m a

-- Given: we want this to be our Monad stack
type AppStack e w s a = ExceptT e (WriterT w (StateT s Effect)) a

-----------------------------------------
-- 15. Write AppM in terms of AppStack --
-----------------------------------------

-- We’ll want our error to be a String, our State to be an Int, our log to be a String
-- and the final result of our application to be Unit.

-- type AppM = ...

----------------------
-- 16. Write runApp --
----------------------

-- Use the type hole to find out about the application result

-- runApp :: Int -> AppM -> Effect _

------------------------------------
-- 17. Factor out the return type --
------------------------------------

-- update runApp with new type alias

-- type StackResult = ...

----------------------------------------------------------------------------------------
-- 18. Write a type alias using a record to store all side-effects of the monad stack --
----------------------------------------------------------------------------------------

-- type AppEffects = ...

-- Given: AppResult contains our side-effect values from running our monad stack, i.e. AppEffects
--        and, optionally, the error if one occurred

-- type AppResult = Tuple (Maybe String) AppEffects

---------------------------------------------------------------------------
-- 19. Write a mapping function that turns StackResult into an AppResult --
---------------------------------------------------------------------------

-- use results function to change runApp

-- results :: StackResult -> AppResult

-------------------------------------
-- 20. Write the application monad --
-------------------------------------

-- the requirements for the body of app in the specified order:

-- write to the log "Starting App..."
-- get the State
-- check the State to make sure it is non-zero, otherwise error with "WE CANNOT HAVE 0 STATE!"
-- add 1 to the State
-- write to the log "Incremented State"
-- return the Pure Computational Value

-- app :: AppM

--------------------------------------------------------------------------------------
-- 21. Write a helper function that adds a newline after every string to the writer --
--------------------------------------------------------------------------------------

-- use function 'log' in function 'app'

-- log :: ...

----------
-- Main --
----------

main :: Effect Unit
main = do
  log "Chapter 21."
  -- result1 <- runApp 0 app
  -- Console.log $ show result1 -- (Tuple Nothing { log: "Starting App...\nIncremented State\n", result: (Just unit), state: 1 })
  -- result2 <- runApp 99 app
  -- Console.log $ show result2 -- (Tuple (Just "WE CANNOT HAVE 0 STATE") { log: "Starting App...\n", result: Nothing, state: 99 })

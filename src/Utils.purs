module Utils where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Reader.Trans (class MonadTrans, lift)
import Data.Either (Either, either)
import Data.Tuple (Tuple(..))
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)

-- for every AVar 'take' there is a 'put'
withAVar :: ∀ a b m. MonadAff m => AVar a -> (a -> m (Tuple a b)) -> m b
withAVar aVar f = do
  value <- liftAff $ AVar.take aVar
  Tuple newValue result <- f value
  liftAff $ AVar.put newValue aVar
  pure result

liftEither :: ∀ e m a. MonadError e m => Either e a -> m a
liftEither = either throwError pure

liftSuccess :: ∀ e a m t. Monad m => MonadTrans t => MonadError e (t m) => m (Either e a) -> t m a
liftSuccess ma = ma # lift >>= liftEither

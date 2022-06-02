module Utils where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, liftEither)
import Control.Monad.Reader.Trans (class MonadTrans, lift)
import Data.Either (Either)
import Data.Tuple (Tuple(..))
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)

-- for every AVar 'take' there is a 'put'.
--                                                    Tuple a b = Tuple newValue result
withAVar :: ∀ a b m. MonadAff m => AVar a -> (a -> m (Tuple a b)) -> m b
withAVar aVar f = do
  value <- liftAff $ AVar.take aVar
  Tuple newValue result <- f value
  liftAff $ AVar.put newValue aVar
  pure result

liftSuccess :: ∀ t m e a. MonadTrans t => MonadThrow e (t m) => Monad m => m (Either e a) -> t m a
liftSuccess ma = ma # lift >>= liftEither

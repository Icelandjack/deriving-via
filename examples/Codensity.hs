{-# Language RankNTypes, DerivingStrategies, DeriveFunctor, InstanceSigs, ScopedTypeVariables, DerivingVia #-}
module Codensity where

-- import Data.Kind
import Data.Functor.Compose
-- import Control.Monad
import Control.Monad.IO.Class

class MonadTrans trans where
  lift :: Monad m => m a -> trans m a

----------------------------------------------------------------------
-- Stuff
----------------------------------------------------------------------

foo :: Codensity m a -> ContT f m a
foo (Codensity cont) = ContT cont

----------------------------------------------------------------------
-- Codensity
----------------------------------------------------------------------

newtype Codensity m a = Codensity
  { runCodensity :: forall b. (a -> m b) -> m b
  }
  deriving stock Functor

instance Applicative (Codensity f) where
  pure x = Codensity ($ x)
  Codensity f <*> Codensity g = Codensity (\bfr -> f (\ab -> g (bfr . ab)))

instance Monad (Codensity f) where
  return = pure
  m >>= k = Codensity (\c -> runCodensity m (\a -> runCodensity (k a) c))

instance MonadIO m => MonadIO (Codensity m) where
  liftIO = (\m -> Codensity (m >>=)) . liftIO

instance MonadTrans Codensity where
  lift :: forall m a. Monad m => m a -> Codensity m a
  lift m = Codensity bind

    where
    bind :: forall xx. (a -> m xx) -> m xx
    bind = (m >>=)

----------------------------------------------------------------------
-- ContT f
----------------------------------------------------------------------

newtype ContT f m a = ContT (forall xx. (a -> m (f xx)) -> m (f xx))
  deriving (Functor, Applicative, Monad)
    via (Codensity (Compose m f))

  -- deriving (MonadTrans)
  --   via Codensity

-- instance MonadTrans (ContT f) where
--   lift :: forall m a. Monad m => m a -> ContT f m a
--   lift m = ContT @_ @m @a (bind @(f _))

--     where
--     bind :: forall xx. (a -> m xx) -> m xx
--     bind = (m >>=)

-- type Run t = forall n b. Monad n => t n b -> n (StT t b)

-- class MonadTrans t => MonadTransControl t where
--   type StT t a :: Type
--   liftWith :: Monad m => (Run t -> m a) -> t m a
--   restoreT :: Monad m => m (StT t a) -> t m a

-- -- instance (Traversable f, Monad f) => MonadTransControl (ContT f) where
-- --     type StT (ContT f) a = f a
-- --     liftWith f = lift (f handleContT)
-- --     restoreT = (>>= choose) . lift

-- For more general:

-- newtype W (wrap::(k -> Type)->(k->Type)) f a = W { runW :: forall xx. (a -> wrap f xx) -> wrap f xx }
--   -- deriving (Functor, Applicative, Monad)
--   --   via (Codensity f)

-- instance Functor (W wrap k) where
--   fmap f (W m) = W (\k -> m (k . f))

-- instance Applicative (W wrap f) where
--   pure x = W (\k -> k x)
--   W f <*> W g = W (\bfr -> f (\ab -> g (bfr . ab)))

-- instance Monad (W wrap f) where
--   return = pure
--   m >>= k = W (\c -> runW m (\a -> runW (k a) c))

-- instance MonadReader r (wrap m) => MonadReader r (W wrap m) where
--   ask :: W wrap m r
--   ask = W (ask >>=)

--   local :: (r -> r) -> (W wrap m a -> W wrap m a)
--   local f m = W $ \c -> ask >>= \r -> local f . runW m $ local (const r) . c

-- newtype F m a = F (m a)

-- class Representational1 f where
--   rep1 :: Coercion a b -> Coercion (f a) (f b)

-- instance MonadTrans (W F) where
--   lift :: forall m a. Monad m => m a -> W F m a
--   lift m = W (coerce a) where

--     a :: forall xx. (a -> m xx) -> m xx
--     a = (>>=) m

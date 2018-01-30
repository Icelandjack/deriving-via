{-# Language DeriveFunctor, TypeOperators, InstanceSigs, GeneralizedNewtypeDeriving, RankNTypes, DerivingStrategies, PolyKinds, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, GADTs, DerivingVia #-}

-- We could replace most instances in Control.Monad.State.Class by
--
--   deriving via (W s (ErrorT  e) m) instance (Error e, MonadState s m) => MonadState s (ErrorT e m)
--   deriving via (W s (ExceptT e) m) instance MonadState s m => MonadState s (ExceptT e m)
--   deriving via (W s (ContT   r) m) instance MonadState s m => MonadState s (ContT   r m)
--   deriving via (W s IdentityT   m) instance MonadState s m => MonadState s (IdentityT m)
--   deriving via (W s ListT       m) instance MonadState s m => MonadState s (ListT     m)
--   deriving via (W s MaybeT      m) instance MonadState s m => MonadState s (MaybeT    m)
--   deriving via (W s (ReaderT r) m) instance MonadState s m => MonadState s (ReaderT r m)
--   deriving via (W s (WriterT w) m) instance MonadState s m => MonadState s (WriterT w m)


module MTL where

import Data.Kind
import Control.Monad.State

type f ~> g = forall xx. f xx -> g xx

----------------------------------------------------------------------
-- Monad + Pointed => Applicative / Functor
----------------------------------------------------------------------

class Pointed p where
  point :: a -> p a

newtype WrappedMonad m (a::Type) = WrappedMonad (m a)
  deriving newtype
    (Pointed, Monad)

instance (Monad f, Pointed f) => Functor (WrappedMonad f) where
  fmap f xs = xs >>= \x -> point (f x)

instance (Monad f, Pointed f) => Applicative (WrappedMonad f) where
  pure = point
  ff <*> fx = ff >>= \f -> fx >>= \x -> point (f x)

----------------------------------------------------------------------
-- Adapter
----------------------------------------------------------------------

newtype W :: Type -> ((Type -> Type) -> (Type -> Type)) -> ((Type -> Type) -> (Type -> Type)) where
  W :: trans m a -> (W s) trans m a
  deriving newtype (Functor, Applicative, Pointed, Monad)

instance (Monad (trans m), MonadTrans trans, MonadState s m) => MonadState s (W s trans m) where
  get :: W s trans m s
  get = W (lift get)

  put :: s -> W s trans m ()
  put = W . lift . put

  state :: (s -> (a, s)) -> W s trans m a
  state = W . lift . state

----------------------------------------------------------------------
-- IdentityT
----------------------------------------------------------------------

newtype IdentityT m (a::Type) = IdentityT { runIdentityT :: m a }
  deriving (Functor, Applicative)
    via (WrappedMonad m)

  deriving newtype
    (Monad, Pointed, MonadState s)

instance MonadTrans IdentityT where
  lift :: m ~> IdentityT m
  lift = IdentityT

----------------------------------------------------------------------
-- MaybeT
----------------------------------------------------------------------

-- newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
--   deriving Applicative
--     via (WrappedMonad m)

--   deriving stock
--     (Functor)

-- instance Monad m => Monad (MaybeT m) where
--   return = undefined

-- instance Applicative m => Monad (MaybeT m) where
--   return = undefined

-- instance MonadTrans MaybeT where
--   lift :: Functor f => f ~> MaybeT f
--   lift ma = MaybeT (Just <$> ma)

----------------------------------------------------------------------
-- ListT
----------------------------------------------------------------------
-- newtype ListT m a = ListT { runListT :: m [a] }

-- We can maybe derive this as well:
--
-- instance (Error e, MonadCont m)  => MonadCont (ErrorT e m)               where callCC = Error.liftCallCC         callCC
-- instance MonadCont m             => MonadCont (ExceptT e m)              where callCC = Except.liftCallCC        callCC
-- instance MonadCont m             => MonadCont (IdentityT m)              where callCC = Identity.liftCallCC      callCC
-- instance MonadCont m             => MonadCont (ListT m)                  where callCC = List.liftCallCC          callCC
-- instance MonadCont m             => MonadCont (MaybeT m)                 where callCC = Maybe.liftCallCC         callCC
-- instance MonadCont m             => MonadCont (ReaderT r m)              where callCC = Reader.liftCallCC        callCC
-- instance (Monoid w, MonadCont m) => MonadCont (LazyRWS.RWST r w s m)     where callCC = LazyRWS.liftCallCC'      callCC
-- instance (Monoid w, MonadCont m) => MonadCont (StrictRWS.RWST r w s m)   where callCC = StrictRWS.liftCallCC'    callCC
-- instance MonadCont m             => MonadCont (LazyState.StateT s m)     where callCC = LazyState.liftCallCC'    callCC
-- instance MonadCont m             => MonadCont (StrictState.StateT s m)   where callCC = StrictState.liftCallCC'  callCC
-- instance (Monoid w, MonadCont m) => MonadCont (LazyWriter.WriterT w m)   where callCC = LazyWriter.liftCallCC    callCC
-- instance (Monoid w, MonadCont m) => MonadCont (StrictWriter.WriterT w m) where callCC = StrictWriter.liftCallCC  callCC

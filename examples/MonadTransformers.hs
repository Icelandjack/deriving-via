{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module MonadTransformers where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

newtype Stack m a = Stack (ReaderT Int (StateT Bool (WriterT String m)) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader Int
    , MonadState Bool
    , MonadWriter String
    )
  deriving MonadTrans via (ReaderT Int `ComposeT` StateT Bool `ComposeT` WriterT String)

{-
instance MonadTrans Stack where
  lift = coerce @(forall m a. Monad m =>
                  m a -> (ReaderT Int `ComposeT` StateT Bool `ComposeT` WriterT String) m a)
                @(forall m a. Monad m =>
                  m a -> Stack m a)
                lift
-}

-----

class MFunctor t where
    hoist :: Monad m => (forall a . m a -> n a) -> t m b -> t n b

instance MFunctor (ReaderT r) where
    hoist nat m = ReaderT (nat . runReaderT m)

instance MFunctor (StateT s) where
    hoist nat m = StateT (nat . runStateT m)

instance MFunctor (WriterT w) where
    hoist nat m = WriterT (nat (runWriterT m))

infixr 9 `ComposeT`
newtype ComposeT (f :: (* -> *) -> * -> *) (g :: (* -> *) -> * -> *) m a
  = ComposeT { getComposeT :: f (g m) a }

instance (MFunctor f, MonadTrans f, MonadTrans g) => MonadTrans (ComposeT f g) where
  lift = ComposeT . hoist lift . lift

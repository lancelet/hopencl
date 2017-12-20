{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Language.OpenCL.Module (
    module Language.OpenCL.Host
  , Module(..)
  , ModuleM(..)
  , theKernels
  ) where

import           Language.OpenCL.Host   (CatchIO, CommandQueue, Context,
                                         ContextM (ContextM), Contextual,
                                         Kernel, Lifespan, PoliteT,
                                         QueueM (QueueM), Queued, Wraps,
                                         contextual, preservingPoliteness,
                                         queued, release, retain, runPolite,
                                         with)

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, asks, runReaderT)


--------------------------------------------------------------------------------
-- Modules
--------------------------------------------------------------------------------

data Module
    = Mod
    { context_ ::  Context
    , kernels_ :: [Kernel]
    , queue_   :: CommandQueue
    }

newtype ModuleM t = ModuleM { runModuleM :: PoliteT (ReaderT Module IO) t }
    deriving (Functor, Applicative, Monad, MonadIO, CatchIO, MonadReader Module)

instance Lifespan Module where
    retain  (Mod c ks q) = sequence_ [ retain c,  retain ks,  retain q  ]
    release (Mod c ks q) = sequence_ [ release c, release ks, release q ]

instance Contextual ModuleM where
    contextual (ContextM c) =
        ModuleM $
        preservingPoliteness
        ( \c' -> asks context_ >>= liftIO . runReaderT c' )
        c

instance Queued ModuleM where
    queued (QueueM c) =
        ModuleM $
        preservingPoliteness
        ( \c' ->
              do
                  ctxt <- asks context_
                  q <- asks queue_
                  liftIO (runReaderT (runReaderT c' q) ctxt)
        ) c

instance MonadIO m => Wraps Module ModuleM m where
    with modul comp = liftIO (runReaderT (runPolite (runModuleM comp)) modul)

theKernels :: ModuleM [Kernel]
theKernels = asks kernels_

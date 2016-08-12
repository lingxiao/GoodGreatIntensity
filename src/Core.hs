{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Datatypes of this application
-- | Author  : Xiao Ling
-- | Date    : 8/11/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Core where

import Control.Monad.IO.Class     (MonadIO   , liftIO    )
import Control.Monad.Except       (MonadError, ExceptT   , 
                                   runExceptT, catchError)
import Control.Monad.State.Strict (MonadState, StateT    , 
                                   runStateT , execStateT, 
                                   evalStateT,     modify)

{-----------------------------------------------------------------------------
    I. System Messages
------------------------------------------------------------------------------}

data Message   = Success
               | FE FileError
               | UnknownError
               deriving (Show)

data FileError = FileDoesNotExist    String
               | NonExistentInputDir String
               | NonExistentOutDir   String
               deriving (Show)

{-----------------------------------------------------------------------------
    I. Compuations over file systems
       newtype FileOp = FileOp (ReaderT FilePath (EitherT E (StateT S IO)) a) 
                deriving (Functor, Applicative, Monad, MonadReader FilePath, 
                          MonadEither E, MonadState S, MonadIO)
------------------------------------------------------------------------------}

type FileOp s m = (MonadState s m, MonadIO m, MonadError Message m)  


runOp :: Monad m => 
       StateT s (ExceptT Message m) a -> s -> m (Either Message a)
runOp m s = runExceptT $ evalStateT m s




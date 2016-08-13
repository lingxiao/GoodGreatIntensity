{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes                  #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Datatypes and primitive operations
-- | Author  : Xiao Ling
-- | Date    : 8/11/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Core where

import Control.Monad.State  
import Control.Monad.Except       
import Control.Monad.IO.Class     
import Control.Monad.Trans.Resource



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
   II. Monad Transformer describing common functions needed
       to interact with file systems
------------------------------------------------------------------------------}

-- * A file operation monad transformer
-- * `FileOps`eration is a stateful computation keeping track of state `s`
type FileOpS m s = (MonadState s m , MonadIO m           , 
                    MonadResource m, MonadError Message m,
                    MonadBaseControl IO m                )  

-- * A File Operation with trivial state `()`
type FileOp  m   = FileOpS m ()


{-----------------------------------------------------------------------------
   III. Operations over `FileOpS`
------------------------------------------------------------------------------}

-- * Run a FileOpS `m` with some user specified state `s`
runS :: (Monad m, MonadBaseControl IO m) 
      => ResourceT (StateT s (ExceptT Message m)) a 
      -> s 
      -> m (Either Message a)
runS m s = runExceptT (evalStateT (runResourceT m) s)


-- * Run a FileOp `m` with trivial state ()
-- * Use this when we do not need to keep a state
run :: (Monad m, MonadBaseControl IO m) 
    => ResourceT (StateT () (ExceptT Message m)) a 
    -> m (Either Message a)
run m = runS m ()


-- * A counter to track, ie, how many files we've seen
tick :: FileOpS m Int => m ()
tick = modify succ 

-- * When logging to console, `demark` the
-- * messages with "============="
demark :: FileOpS m s => m ()
demark = liftIO . putStrLn $ foldr (++) mempty 
                           $ (const "-") <$> [1..50] 





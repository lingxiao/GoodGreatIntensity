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
   I. Monad Transformer describing common functions needed
       to interact with file systems
------------------------------------------------------------------------------}

-- * A file operation monad transformer
-- * `FileOps`eration is a stateful computation keeping track of state `s`
type FileOpS m s = (MonadState s m , MonadIO m            , 
                    MonadResource m, MonadBaseControl IO m)

-- * A File Operation with trivial state `()`
type FileOp  m   = FileOpS m ()

{-----------------------------------------------------------------------------
   II. Operations over `FileOpS`
------------------------------------------------------------------------------}

-- * Run a FileOpS `m` with some user specified state `s`
run' :: (Monad m, MonadBaseControl IO m) 
      => ResourceT (StateT s m) a 
      -> s 
      -> m a
run' m s = evalStateT (runResourceT m) s

-- * Run a FileOp `m` with trivial state ()
-- * Use this when we do not need to keep a state
run :: (Monad m, MonadBaseControl IO m) 
    => ResourceT (StateT () m) a 
    -> m a
run m = run' m ()


-- * When logging to console, `demark` the
-- * messages with "============="
banner :: IO ()
banner = putStrLn $ foldr (++) mempty 
                  $ (const "-") <$> [1..50] 
































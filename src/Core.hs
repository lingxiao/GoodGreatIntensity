{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
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


import Control.Monad.Except         (MonadError      , ExceptT   , 
                                     runExceptT      , catchError)
import Control.Monad.IO.Class       (MonadIO         , liftIO    )
import Control.Monad.State.Strict   (MonadState      , StateT    , 
                                     runStateT       , execStateT, 
                                     evalStateT      , modify    )
import Control.Monad.Trans.Resource (MonadResource   , ResourceT , 
                                     MonadBaseControl, runResourceT)

import Data.ByteString              (ByteString          )
import Data.Conduit hiding          ((=$), ($=)          )  
import Data.Conduit.Binary          (sourceFile, sinkFile)
import Data.Conduit.Filesystem      (sourceDirectory     )


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
   II. File system monad transformer
------------------------------------------------------------------------------}

-- * File operation monad transformer constraints
-- * TODO: add ConduitM instance
type FileOpS s m = (MonadState s m, MonadIO m, MonadResource m, MonadError Message m)  
type FileOp m    = FileOpS () m

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


path = "/Users/lingxiao/Documents/NLP/Code/Papers/GoodGreatIntensity/src/sample.txt"

output :: FileOp m => Consumer ByteString m ()
output = do
    xs <- await
    liftIO . print . show $ xs
    return ()

bar :: FileOp m => ConduitM a c m ()
bar = sourceFile path =$= output

baz :: FileOp m => m ()
baz = sourceFile path $$ output

foo :: FileOp m => m ()
foo = runConduit bar






-- * now open a zip file
-- * decode and print to console















{-


{-----------------------------------------------------------------------------
    II. Compuations type over file systems
------------------------------------------------------------------------------}


-- * Run a FileOpS `m` with some user specified state `s`
run :: Monad m => 
       StateT s (ExceptT Message m) a -> s -> m (Either Message a)
run m = runExceptT . evalStateT m

-- * Run a FileOp `m` with trivial state ()
-- * Use this when we do not need to keep a state
run' :: Monad m => StateT () (ExceptT Message m) a -> m (Either Message a)
run' m = run m ()

{-----------------------------------------------------------------------------
    III. Primitive `FileOpS` operations
------------------------------------------------------------------------------}

tick :: FileOpS Int m => m ()
tick = modify succ 

{-----------------------------------------------------------------------------
    IV. Primitive Operations over file systems
------------------------------------------------------------------------------}

path = "/Users/lingxiao/Documents/NLP/Code/Papers/GoodGreatIntensity/src/sample.txt"

-- * read file using conduit's sourceFile
-- * if filepath invalid then throw error
--openFile :: FileOp m => m ()
--openFile = don
    --f <- liftIO $ sourceFile path
    --return ()

-- * Open up a zip file from source and unzip
openZip :: FileOp m => m ()
openZip = undefined


saveFile :: FileOp m => m ()
saveFile = undefined


--traverse :: FileOpS s m => Bool -> FilePath -> Producer m FilePath
--traverse = undefined

openF :: FileOpS Int m => m ()
openF = do
   f <- liftIO $ readFile path
   tick
   liftIO $ print f
   return ()




-}


























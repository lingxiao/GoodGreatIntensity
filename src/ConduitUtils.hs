{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes                  #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Utility functions defined over Conduits
-- | Author  : Xiao Ling
-- | Date    : 8/12/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module ConduitUtils where

import Control.Exception.Base
import Control.Monad.State  
import Control.Monad.Except         (MonadError      , ExceptT   , 
                                     runExceptT      , catchError)
import Control.Monad.IO.Class       (MonadIO         , liftIO    )
import Control.Monad.Trans.Resource (MonadResource   , ResourceT , 
                                     MonadBaseControl, runResourceT)

import Control.Exception.Base       (SomeException)

import Data.ByteString              (ByteString          )
import Data.Conduit hiding          ((=$), ($=), ($$)    )  
import Data.Conduit.Binary          (sourceFile, sinkFile)
import Data.Conduit.Filesystem      (sourceDirectory     )

import Core

{-----------------------------------------------------------------------------
   toy examples
------------------------------------------------------------------------------}

path  = "/Users/lingxiao/Documents/NLP/Code/Papers/GoodGreatIntensity/src/sample.txt"
path' = "/Users/lingxiao/Documents/NLP/Code/Papers/GoodGreatIntensity/src/"

-- * open one file
foo :: FileOpS m Int => m ()
foo = runConduit $ sourceFile path =$= logNum =$= logData =$= cap

-- * stream files in directory
bar :: FileOpS m Int => m ()
bar = runConduit $ sourceDirectory path' =$= idc =$= logData =$= cap

-- * note this whole thing crashes
err :: FileOp m => m ()
err = runConduit $ sourceFile "" =$= logData =$= cap

-- * trivially: make a sourceFile output "" if file not found
sourceFileE :: (MonadBaseControl IO m, Exception e) => FilePath -> Source m ByteString
sourceFileE f = catchC (sourceFile f) 
                :(\(e :: SomeException) -> yield mempty)


-- * now stream all files and print 
-- * if a file extension does not exist then skip over it
baz :: FileOpS m s => Conduit i m o
baz = undefined



{-----------------------------------------------------------------------------
   Conduit Utility Functions
------------------------------------------------------------------------------}


-- * open zip file and unzip

-- * Log the current index of file `mxs` encountered
logNum :: (Show i, FileOpS m Int) => Conduit i m i
logNum = awaitForever $ \xs -> do
                lift tick
                n <- lift get
                demark   >> ping n
                yield xs >> logNum
                  where ping n = liftIO . putStrLn $ "file number " ++ show n


-- * Print actual data `xs` to console
logData :: (Show i, FileOpS m s) => Conduit i m i
logData = awaitForever $ \xs -> do
                demark   >> (liftIO . putStrLn . show $ xs)
                yield xs >> logData


-- * `cap` a conduit pipeline 
cap :: FileOpS m s => Consumer i m ()
cap = do
  mx <- await
  case mx of
    Nothing -> do
      demark
      liftIO $ putStrLn "pipe terminated"
      demark
      return ()
    _       -> cap


-- * identity
idc :: Monad m => Conduit i m i 
idc = awaitForever $ \xs -> yield xs >> idc













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


























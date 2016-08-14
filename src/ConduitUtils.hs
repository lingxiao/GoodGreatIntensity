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

import System.FilePath.Posix        (takeExtension       )

import Control.Monad.State  
import Control.Monad.IO.Class       (MonadIO, liftIO     )
import Control.Exception.Base       (SomeException       )

import Conduit                      (mapM_C)
import Data.Conduit hiding          ((=$), ($=), ($$)    )  
import Data.Conduit.Binary          (sourceFile, sinkFile)
import Data.Conduit.Filesystem      (sourceDirectory     )
import Data.ByteString              (ByteString          )

import Core

{-----------------------------------------------------------------------------
   toy examples
------------------------------------------------------------------------------}

path  = "/Users/lingxiao/Documents/NLP/Code/Papers/GoodGreatIntensity/src/sample.txt"
path' = "/Users/lingxiao/Documents/NLP/Code/Papers/GoodGreatIntensity/src/"
path2 = "/Users/lingxiao/Documents/NLP/Code/Papers/dummydata/txt/"

-- * open one file
foo :: FileOpS m Int => m ()
foo = runConduit $ sourceFile path =$= logNum =$= logData =$= cap

-- * stream files in directory
bar :: FileOpS m Int => m ()
bar = runConduit $ sourceDirectory path' =$= idc =$= logData =$= cap

-- * note this whole thing crashes
err :: FileOp m => m ()
err = runConduit $ sourceFile "" =$= logData =$= cap

-- * now stream all files and print 
-- * if a file extension does not exist then skip over it
err2 :: FileOp m => m ()
err2 = runConduit $ sourceFileE "" =$= logData =$= cap

-- * now shallow traverse all files and open, if file does not exist then 
-- * then output empty bytestring

-- * where you left off: so now you need to 
-- * just go ahead and make a function that counts specific occurences 
-- * of something and ouput its count
baz :: FileOpS m Int => m ()
baz = runConduit $ sourceDirectory path2 =$= 
      mapM_C (\p -> liftIO $ print . show . length $ p)


--baz = runConduit $ sourceDirectory path2 =$= logNum =$= logData =$= cap
      -- =$= mapM_C (\p -> print . show $ p )

-- sourceDirectory "." =$= mapMC (\path -> liftIO $ fmap B.length $ B.readFile path)


{-

Data.ByteString as B

runResourceT $  sourceDirectory "." 
             $$ filterC (\path -> 
             takeExtension path == ".txt") 
             =$= mapMC (\path -> 
             liftIO $ fmap B.length $ B.readFile path) =$= foldlC (+) 0
-}

{-----------------------------------------------------------------------------
   Conduit Utilities
------------------------------------------------------------------------------}

-- * open zip file and unzip

-- * if no file exists at `FilePath` `f`, then
-- * output empty ByteString
sourceFileE :: FileOpS m s
            => FilePath -> Source m ByteString
sourceFileE f = catchC (sourceFile f) 
                (\(e :: SomeException) -> yield mempty)


-- * Log the current index of file `mxs` encountered
-- * for each new file encountered, increment the counter
logNum :: (Show i, FileOpS m Int) => Conduit i m i
logNum = awaitForever $ \xs -> do
                lift . modify $ succ
                n <- lift get
                liftIO demark
                log_ n
                yield xs
                logNum
                  where 
                    log_ n = liftIO . putStrLn 
                           $ "file number " ++ show n


-- * Print actual data `xs` to console
logData :: (Show i, FileOpS m s) => Conduit i m i
logData = awaitForever $ \xs -> do
                liftIO demark
                liftIO . putStrLn . show $ xs
                yield xs
                logData


-- * `cap` a conduit pipeline 
cap :: FileOpS m s => Consumer i m ()
cap = do
  mx <- await
  case mx of
    Nothing -> do
      liftIO demark
      liftIO $ putStrLn "pipe terminated"
      liftIO demark
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


























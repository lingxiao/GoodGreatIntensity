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

import Prelude hiding               (readFile            )
import System.FilePath.Posix        

import Control.Monad.State  
import Control.Monad.IO.Class       (MonadIO, liftIO     )
import Control.Exception.Base       (SomeException       )

import Conduit                      (mapM_C, filterC     ,
                                     foldlC, foldC       )
import Data.Conduit hiding          ((=$), ($=),         )
import Data.Conduit.Binary          (sourceFile, sinkFile)
import Data.Conduit.Filesystem      (sourceDirectory     )
import Data.ByteString              (ByteString, readFile)
import qualified Data.ByteString.Char8 as B

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

countLines :: FileOpS m s => FilePath -> m ()
countLines p = do
  xs <- liftIO $ readFile p
  liftIO . print . show . length . B.lines $ xs
  return ()



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


-- * Shallow traversal of all files in path `p` with extension `e`
-- * If path invalid or extension invalid, pipe terminates
sourceDirExt :: FileOpS m s => FilePath -> String -> Source m FilePath
sourceDirExt p e =   sourceDirectory (takeDirectory p) 
                 =$= filterC (\p -> takeExtension p == e)

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





























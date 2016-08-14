{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Conduit Functions. These form a poor man's DSL for this application
-- | Author  : Xiao Ling
-- | Date    : 8/12/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module ConduitLib where

import Prelude hiding               (readFile            )
import System.FilePath


import Control.Monad.State  
import Control.Monad.IO.Class       (MonadIO, liftIO     )
import Control.Exception.Base       (SomeException       )

import Data.Conduit 
import Conduit hiding               (sourceDirectory     ,
                                     sourceFile          )

import Data.Conduit.Binary          (sourceFile, sinkFile)
import Data.Conduit.Filesystem      (sourceDirectory     )
import Data.ByteString              (ByteString, readFile)
import qualified Data.ByteString.Char8 as B

import Core

{-----------------------------------------------------------------------------
   Conduit File system combinators
------------------------------------------------------------------------------}

-- * open zip file and unzip


-- * Shallow traversal of all files in path `p` with extension `e`
-- * If path invalid or extension invalid, pipe terminates
traverseAll :: FileOpS m s => FilePath -> String -> Source m FilePath
traverseAll p e =   sourceDirectory (takeDirectory p) 
               =$= filterC (\p -> takeExtension p == e)

-- * if no file exists at `FilePath` `f`, then
-- * output empty ByteString
sourceFileE :: FileOpS m s
            => FilePath -> Source m ByteString
sourceFileE f = catchC (sourceFile f) 
                (\(e :: SomeException) -> yield mempty)


{-----------------------------------------------------------------------------
   Conduit logging functions
------------------------------------------------------------------------------}

-- * Log the current index of file `mxs` encountered
-- * for each new file encountered, increment the counter
logNum :: (Show i, FileOpS m Int) => Conduit i m i
logNum = awaitForever $ \xs -> do
                lift . modify $ succ
                n <- lift get
                liftIO banner
                log_ n
                yield xs
                logNum
                  where 
                    log_ n = liftIO . putStrLn 
                           $ "file number " ++ show n

-- * log input `i` to console
logi :: (Show i, FileOpS m s) => Conduit i m i
logi = awaitForever $ \xs -> do
                liftIO banner
                liftIO . putStrLn . show $ xs
                yield xs
                logi


{-----------------------------------------------------------------------------
   Other
------------------------------------------------------------------------------}

-- * count the number of lines in file `f`
countLines :: FileOpS m s => Conduit ByteString m Int
countLines = awaitForever $ \f -> yield (length . B.lines $ f)


-- * `cap` a conduit pipeline 
cap :: FileOpS m s => Consumer i m ()
cap = do
  mx <- await
  case mx of
    Nothing -> do
      liftIO banner
      liftIO $ putStrLn "pipe terminated"
      liftIO banner
      return ()
    _       -> cap


-- * identity
idc :: Monad m => Conduit i m i 
idc = awaitForever $ \xs -> yield xs >> idc





























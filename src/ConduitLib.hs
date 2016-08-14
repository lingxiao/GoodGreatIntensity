{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : A library of conduit functions, forming a poor man's DSL 
-- | Author  : Xiao Ling
-- | Date    : 8/12/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module ConduitLib where

import Prelude hiding           (readFile            )
import System.FilePath
import System.Directory


import Control.Monad.State  
import Control.Monad.IO.Class   (MonadIO, liftIO     )
import Control.Exception.Base   (SomeException       )

import Data.Conduit 
import Conduit hiding           (sourceDirectory     ,
                                 sourceFile          )

import Data.Conduit.Filesystem  (sourceDirectory     )
import Data.Conduit.Binary      (sourceFile, sinkFile)
import Data.ByteString          (ByteString, readFile)
import Codec.Compression.GZip   (decompress          )
import qualified Data.ByteString.Char8 as B

import qualified Data.ByteString.Lazy as L

import Core

{-----------------------------------------------------------------------------
   Conduit routines
------------------------------------------------------------------------------}

-- * Untar all files with extension `e1` found at directory `p`
-- * and save them in the same directory with extension `e2 
untarAll :: FileOpS m s => FilePath -> String -> String -> m ()
untarAll p e1 e2 =  p `traverseAll` e1
                $$  untarSaveAs ".txt"
                =$= cap

{-----------------------------------------------------------------------------
  Conduit sources
------------------------------------------------------------------------------}

-- * Shallow traversal of all files in path `p` with extension `e`
-- * If path invalid or extension invalid, pipe terminates
traverseAll :: FileOpS m s => FilePath -> String -> Source m FilePath
traverseAll p e =   sourceDirectory p
               =$= filterC (\p -> takeExtension p == e)

-- * if no file exists at `FilePath` `f`, then
-- * output empty ByteString
sourceFileE :: FileOpS m s => FilePath -> Source m ByteString
sourceFileE f = catchC (sourceFile f) 
                (\(e :: SomeException) -> yield mempty)

{-----------------------------------------------------------------------------
   Conduit pipes
------------------------------------------------------------------------------}

-- * TODO: swap out the L.readFile for something more safe
-- *       what about   L.writeFile ?

-- * open zip file found at path `p`
-- * and untar it, save it in the same directory with extension `ext`
-- * yield the untared file `f` downstream with its filepath
untarSaveAs :: FileOpS m s => String 
             -> Conduit FilePath m (FilePath, L.ByteString)
untarSaveAs ext = awaitForever $ \p -> do

  let name = dropExtension . takeFileName $ p

  liftIO banner
  liftIO . print $ "untar and save file: " ++ name
  
  f <- liftIO $ decompress <$> L.readFile p   

  liftIO . flip L.writeFile f $ takeDirectory p ++ "/" ++ name ++ ext

  yield (p, f)


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

-- * log message `xs` 
logm :: FileOpS m s => String -> Conduit i m i
logm xs = awaitForever $ \f -> do
  liftIO . putStrLn $ xs
  yield f

-- * count the number of lines in file `f`
countLines :: FileOpS m s => Conduit ByteString m Int
countLines = awaitForever $ \f -> yield (length . B.lines $ f)


-- * identity
idc :: Monad m => Conduit i m i 
idc = awaitForever $ \xs -> yield xs >> idc



{-----------------------------------------------------------------------------
   Conduit sinks
------------------------------------------------------------------------------}

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






























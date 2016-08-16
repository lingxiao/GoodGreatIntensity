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

import Prelude hiding           (readFile, writeFile )
import System.FilePath
import System.Directory


import Control.Monad.State  
import Control.Monad.IO.Class   (MonadIO, liftIO     )
import Control.Exception.Base   (SomeException       )

import Codec.Compression.GZip   (decompress          )

import Data.List.Split          (chunksOf            )
import Data.ByteString          (ByteString          )
import Data.Conduit 
import Conduit hiding           (sourceDirectory     ,
                                 sourceFile          )
import Data.Conduit.Filesystem  (sourceDirectory     )
import Data.Conduit.Binary      (sourceFile, sinkFile)
import Data.Text.Lazy.IO        (readFile , writeFile)
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as L

import Core

{-----------------------------------------------------------------------------
   Conduit routines
------------------------------------------------------------------------------}

-- * Untar all files with extension `e1` found at directory `p`
-- * and save them in the same directory with extension `e2 
untarAll :: FileOpS m s => FilePath -> String -> String -> m ()
untarAll p e1 e2 =  p `traverseAll` e1
                $$  untarSaveAs e2
                =$= logm "untarred all files!"
                =$= cap


-- * Shard all files with `ext` found at directory `p`
-- * into chunks of 100000 lines each
-- * and save in output directory `o`
shardAll :: FileOpS m s => String -> FilePath -> FilePath -> m ()
shardAll ext p o =  p `traverseAll` ext
                $$  shardFile ext o 100000
                =$= logm "Sharded all files!"
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
untarSaveAs :: FileOpS m s
            => String 
            -> Conduit FilePath m (FilePath, L.ByteString)
untarSaveAs ext = awaitForever $ \p -> do

  let name = dropExtension . takeFileName $ p

  liftIO banner
  liftIO . print $ "untar and save file: " ++ name
  
  f <- liftIO $ decompress <$> L.readFile p   

  liftIO . flip L.writeFile f $ takeDirectory p ++ "/" ++ name ++ ext

  yield (p, f)


-- * Shard all file with extension `ext` found at path `p` into 
-- * `size`ed pieces and save in output directory `o`
shardFile :: FileOpS m s 
          => String 
          -> FilePath 
          -> Int 
          -> Conduit FilePath m ()
shardFile ext o size = awaitForever $ \p -> do
    
    let name = dropExtension . takeFileName $ p
    let dir  = takeExtension p

    f <- liftIO $ readFile p
    let ts  = LT.splitOn (LT.pack "\n") f
    let ts' = LT.unlines <$> chunksOf size ts

    liftIO $ foldM 
           (\n t -> do
                let name' = name ++ show n ++ ext
                let path  = o    ++ name'

                banner 
                print $ "saving file: " ++ path

                writeFile path t
                return $ n + 1

            ) 0 ts'

    return ()

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
  liftIO banner
  liftIO . putStrLn $ xs
  liftIO banner
  yield f


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






























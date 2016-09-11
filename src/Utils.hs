{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Misc scripts to operate on files
-- | Author  : Xiao Ling
-- | Date    : 8/27/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Utils (
      untar
    , shardAll
    , concatFiles
    , cutFiles
    ) where


import System.Directory
import System.FilePath.Posix
import qualified System.IO as S
import Data.Conduit 
import Data.List.Split

import Core
import Lib



{-----------------------------------------------------------------------------
   Untar files
------------------------------------------------------------------------------}

-- * @Use: run $ untar "/path/to/file" ".gz" ".txt"
-- * Untar all files with extension `e1` found at directory `p`
-- * and save them in the same directory with extension `e2` 
untar :: FileOpS m s => FilePath -> String -> String -> m ()
untar p e1 e2 =  [p] `sourceDirectories` e1
                $$  untarSaveAs e2
                =$= cap


-- * Shard all files with `ext` found at directory `p`
-- * into chunks of 100000 lines each
-- * and save in output directory `o`
shardAll :: FileOpS m s => String -> FilePath -> FilePath -> m ()
shardAll ext p o =  [p] `sourceDirectories` ext
                $$  shardFile ext o 100000
                =$= logm "Sharded all files!"
                =$= cap                

{-----------------------------------------------------------------------------
   untar all files
------------------------------------------------------------------------------}

--untarAll :: IO ()
--untarAll = do
--    run $ untar p1 ".gz" ".txt"
--    run $ untar p2 ".gz" ".txt"
--    run $ untar p3 ".gz" ".txt"
--    run $ untar p4 ".gz" ".txt"
--    run $ untar p5 ".gz" ".txt"

{-----------------------------------------------------------------------------
  concat all files
------------------------------------------------------------------------------}

concatFiles :: DirectoryPath -> FilePath -> IO ()
concatFiles d f = do
  fs   <- sourceDirs ".txt" [d]
  file <- sequence $ readFile <$> fs
  let ts   = concat file
  let path = f ++ "/" ++ "catGrams.txt"

  o <- S.openFile path S.WriteMode
  S.hPutStrLn o ts
  S.hClose o
  return ()

{-----------------------------------------------------------------------------
  downsize all files
------------------------------------------------------------------------------}

cutFiles :: DirectoryPath -> IO FilePath
cutFiles d = do
  let dir   = takeDirectory d
  let name' = takeBaseName  d
  let name  = name' ++ "_copy"
  let d'    = dir ++ "/" ++ name
  createDirectoryIfMissing False d'

  fs <- sourceDir ".txt" d
  mapM (\f -> cutFile d' f) fs
  return d'


-- * open file found at `f`, truncate and save in directory `d`
cutFile :: DirectoryPath -> FilePath -> IO [String]
cutFile d f = do
    xs <- readFile f

    let ys   = splitOn "\n" xs
    --let ys'  = takeN (length ys) ys
    let ys' = ys
    let name = (takeBaseName . takeFileName $ f) ++ ".txt"
    let out  = d ++ "/" ++ name

    h  <- S.openFile out S.WriteMode
    mapM (S.hPutStrLn h) ys'
    S.hClose h

    return ys'

takeN :: Int -> [a] -> [a]
takeN 0 _      = []
takeN n (x:xs) = x : takeN (n-1) xs
takeN _ _      = []


projr = "/home1/l/lingxiao/xiao/GoodGreatIntensity/"
datar = "/nlp/data/xiao/ngrams/"

f1r, f4r, f4sr, f5r, fdr, fnr :: DirectoryPath
f1r = datar ++ "1gms"
f4r = datar ++ "4gms"
f4sr = datar ++ "4gms_small"
f5r = datar ++ "5gms"
fdr = datar ++ "dummydata"
fnr = datar ++ "ngrams"





























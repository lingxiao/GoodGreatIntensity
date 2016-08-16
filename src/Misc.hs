{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Random routines that came in hand
-- | Author  : Xiao Ling
-- | Date    : 8/15/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Misc where


import Prelude hiding           (readFile, writeFile )

import Data.List.Split
import Data.Text.Lazy.IO        
import qualified Data.Text.Lazy as L

import Core
import ConduitLib
import NgramParser


p1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/1gms/"
p = p1 ++ "vocab.txt" :: FilePath


{-----------------------------------------------------------------------------
   Shard vocab.txt into 135 files 
------------------------------------------------------------------------------}


shardVocab :: IO ()
shardVocab = do
    f <- readFile p
    let ys  = L.splitOn (L.pack "\n") f
    let ys' = L.unlines <$> chunksOf 100000 ys
    saveAll 0 ys'
    return ()

saveAll :: Int -> [L.Text] -> IO ()    
saveAll _ []     = return ()
saveAll n (t:ts) = do
    let name = "1gms-" ++ show n ++ ".txt"
    writeFile (p1 ++ name) t
    saveAll (n+1) ts


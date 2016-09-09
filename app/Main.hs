{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Main
-- | Author  : Xiao Ling
-- | Date    : 8/10/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
 
module Main where


import System.Directory
import Data.List.Split

import Core
import MainSubroutines


main :: IO ()
main = do
    f <- getCurrentDirectory
    let top:_ = splitOn "GoodGreatIntensity" f
    if top == "/Users/lingxiao/Documents/NLP/Code/" then 
        mainLocal
    else 
        mainRemote

{-----------------------------------------------------------------------------
  Local and remote main
------------------------------------------------------------------------------}

mainLocal :: IO ()
mainLocal = do
    main_p1 cLocal
    main_p2 cLocal

mainRemote :: IO ()
mainRemote = do
    main_p1 cRemote
    main_p2 cRemote


{-----------------------------------------------------------------------------
  System Paths
------------------------------------------------------------------------------}

cLocal :: Config
cLocal = Con f1 [f4,f5] fsw fws

cRemote :: Config
cRemote = Con f1r [f5d] fswr fwsr


-- * local ngram directory
f1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/1gms"
f4 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/4gms"
f5 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/5gms"
-- * local dummy data directory
fd = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/dummydata"
-- * local pattern directory
fsw = "/Users/lingxiao/Documents/NLP/Code/GoodGreatIntensity/inputs/strong-weak-patterns.txt"
fws = "/Users/lingxiao/Documents/NLP/Code/GoodGreatIntensity/inputs/weak-strong-patterns.txt"

-- * remote ngram directory
f1r, f4r, f5r :: FilePath
f1r = "/nlp/data/xiao/ngrams/1gms"
f4r = "/nlp/data/xiao/ngrams/4gms"
f5r = "/nlp/data/xiao/ngrams/5gms"
f5d = "/nlp/data/xiao/ngrams/dummydata"
-- * remote pattern path
fswr = "/home1/l/lingxiao/xiao/GoodGreatIntensity/inputs/strong-weak-patterns.txt"
fwsr = "/home1/l/lingxiao/xiao/GoodGreatIntensity/inputs/weak-strong-patterns.txt"




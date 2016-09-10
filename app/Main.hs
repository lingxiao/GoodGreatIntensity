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


{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}

main :: IO ()
main = do
    f <- getCurrentDirectory
    let top:_ = splitOn "GoodGreatIntensity" f
    if top == "/Users/lingxiao/Documents/NLP/Code/" then 
        mainAt local
    else 
        mainAt remote


mainAt :: Config -> IO ()
mainAt con = do
    main_test con

{-----------------------------------------------------------------------------
  System Paths
------------------------------------------------------------------------------}

local :: Config
local = Con f1 [fd] fsw fws

remote :: Config
remote = Con f1r [f4r] fswr fwsr

projl = "/Users/lingxiao/Documents/NLP/Code/GoodGreatIntensity/"
datal = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/"

projr = "/home1/l/lingxiao/xiao/GoodGreatIntensity/"
datar = "/nlp/data/xiao/ngrams/"

-- * local ngram directory
f1,f4,f5,fd :: FilePath
f1 = datal ++ "1gms"
f4 = datal ++ "4gms"
f5 = datal ++ "5gms"
fd = datal ++ "dummydata"

-- * local pattern directory
fsw, fws :: FilePath
fsw = projl ++ "inputs/strong-weak-patterns.txt"
fws = projl ++ "inputs/weak-strong-patterns.txt"

-- * remote ngram directory
f1r, f4r, f5r, fdr :: FilePath
f1r = datar ++ "1gms"
f4r = datar ++ "4gms"
f5r = datar ++ "5gms"
fdr = datar ++ "dummydata"

-- * remote pattern path
fswr, fwsr :: FilePath
fswr = projr ++ "inputs/strong-weak-patterns.txt"
fwsr = projr ++ "inputs/weak-strong-patterns.txt"





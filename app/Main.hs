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




import Data.Text (Text, pack, unpack)

import Lib
import Src
import MainSubroutines


{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}

main :: IO ()
main = do
    f <- getCurrentDirectory
    let top:_ = splitOn "GoodGreatIntensity" f
    if top == "/Users/lingxiao/Documents/NLP/Code/" then 
        runFrom local
    else 
        runFrom remote


runFrom :: Config -> IO ()
runFrom con = do
    main_test con
    --let d:_ = ngrams con
    --cutFiles d
    --return ()



local :: Config
local = Con f1 [f4] fsw fws

remote :: Config
remote = Con f1r [fnr] fswr fwsr


{-----------------------------------------------------------------------------
  Local Paths
------------------------------------------------------------------------------}

projl = "/Users/lingxiao/Documents/NLP/Code/GoodGreatIntensity/"
datal = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/"

-- * ngrams
f1,f4,f5,fd :: DirectoryPath
f1 = datal ++ "1gms"
f4 = datal ++ "4gms"
f5 = datal ++ "5gms"
fd = datal ++ "dummydata"

-- * patterns
fsw, fws :: FilePath
fsw = projl ++ "inputs/strong-weak-patterns.txt"
fws = projl ++ "inputs/weak-strong-patterns.txt"

{-----------------------------------------------------------------------------
  Remote Paths
------------------------------------------------------------------------------}



projr = "/home1/l/lingxiao/xiao/GoodGreatIntensity/"
datar = "/nlp/data/xiao/ngrams/"

f1r, f4r, f4sr, f5r, fdr, fnr :: DirectoryPath
f1r  = datar ++ "1gms"
f4r  = datar ++ "4gms"
f4sr = datar ++ "4gms_small"
f4cr = datar ++ "4gms_copy"
f5r  = datar ++ "5gms"
fdr  = datar ++ "dummydata"
fnr  = datar ++ "ngrams"

-- * remote pattern path
fswr, fwsr :: FilePath
fswr = projr ++ "inputs/strong-weak-patterns.txt"
fwsr = projr ++ "inputs/weak-strong-patterns.txt"





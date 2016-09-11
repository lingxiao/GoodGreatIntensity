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
import Subroutines


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
    --main_test con
    main_shard 10000 con


local :: Config
local = Con f1 [f4] fsw fws

remote :: Config
remote = Con r1gms [r4gms_small] fswr fwsr


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


r1gms       = datar ++ "1gms"
r4gms       = datar ++ "4gms"
r4gms_small = datar ++ "4gms_small"
r4gms_copy  = datar ++ "4gms_copy"
r5gms       = datar ++ "5gms"
rdummydata  = datar ++ "dummydata"
rngrams     = datar ++ "ngrams"

-- * remote pattern path
fswr, fwsr :: FilePath
fswr = projr ++ "inputs/strong-weak-patterns.txt"
fwsr = projr ++ "inputs/weak-strong-patterns.txt"





{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : A module to unzip and concactenate .gz files
-- | Author  : Xiao Ling
-- | Date    : 8/11/2016
-- | Note    : check swaped file usage:   sysctl vm.swapusage
-- |           https://hackage.haskell.org/package/zip-conduit
-- |           https://hackage.haskell.org/package/zip
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
 

module CatFiles where


import Prelude hiding             (writeFile, readFile, concat)

import Control.Monad
import Control.Monad.State        (StateT, modify, liftIO, runStateT)
import System.Directory           (getDirectoryContents, doesDirectoryExist)
import Data.ByteString.Lazy.Char8 (ByteString, writeFile, concat)
import Codec.Compression.GZip     (decompress)

import Core


{-----------------------------------------------------------------------------
    Refactor with mtl
------------------------------------------------------------------------------}

path = "/Users/lingxiao/Documents/NLP/Code/Papers/GoodGreatIntensity/src/sample.txt"

tick :: FileOpS Int m => m ()
tick = modify succ 


{-----------------------------------------------------------------------------
    Opening and 
------------------------------------------------------------------------------}



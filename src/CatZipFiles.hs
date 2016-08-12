{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : A module to unzip and concactenate .gz files
-- | Author  : Xiao Ling
-- | Date    : 8/11/2016
-- | Note    : check swaped file usage:   sysctl vm.swapusage
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
 

module CatFiles where


import Prelude hiding             (writeFile, concat)

import Control.Monad
import Control.Monad.State        (StateT, modify, liftIO, runStateT)
import System.Directory           (getDirectoryContents, doesDirectoryExist)
import Data.ByteString.Lazy.Char8 (ByteString, writeFile, concat)
import Codec.Compression.GZip     (decompress)

import Core
import Utils





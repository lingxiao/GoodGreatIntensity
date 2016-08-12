{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Utility Functions
-- | Author  : Xiao Ling
-- | Date    : 8/11/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Utils where

import Core

import Prelude hiding             (readFile)
import System.Directory           (getDirectoryContents)
import Control.Exception.Base     (SomeException, tryJust)
import Data.ByteString.Lazy.Char8 (ByteString, readFile)


{-----------------------------------------------------------------------------
    I. Opening directories and reading Files
       These are non exception throwing versions of 
        similarly named functions
------------------------------------------------------------------------------}


readFile' :: FilePath -> IO (Either (FileError FilePath) ByteString)
readFile' f = tryJust 
              (\(e :: SomeException) -> pure $ FileDoesNotExist f)
              (readFile f)


getDirContents :: FilePath -> IO (Either (FileError FilePath) [FilePath])
getDirContents f = tryJust
                    (\(e :: SomeException) -> pure $ NonExistantInputDir f)
                    (getDirectoryContents f)


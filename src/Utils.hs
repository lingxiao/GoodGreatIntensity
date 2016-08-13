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
    I. Common file path routines
------------------------------------------------------------------------------}

-- * if : `inPath` exist
-- *      `outPath` exist
-- *      `outName` is valid
-- * then output all file paths 
seePaths :: FilePath -> IO (Either Message [FilePath])
seePaths inPath = do
    mfs <- getDirContents inPath
    case mfs of
        Left e   -> return . Left . FE $ e
        Right fs -> return . return $ fs

{-----------------------------------------------------------------------------
    II. Opening directories and reading Files primitives
       These are non exception throwing versions of similarly named functions
------------------------------------------------------------------------------}

readFile' :: FilePath -> IO (Either FileError ByteString)
readFile' f = tryJust 
              (\(e :: SomeException) -> pure $ FileDoesNotExist f)
              (readFile f)


getDirContents :: FilePath -> IO (Either FileError [FilePath])
getDirContents f = tryJust
                    (\(e :: SomeException) -> pure $ NonExistentInputDir f)
                    (getDirectoryContents f)

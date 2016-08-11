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
import Control.Exception.Base     (SomeException, tryJust)
import Data.ByteString.Lazy.Char8 (ByteString, readFile)


{-----------------------------------------------------------------------------
    I. Reading Files
------------------------------------------------------------------------------}

readFile' :: FilePath -> IO (Either (Maybe FileError) (Maybe ByteString))
readFile' f = tryJust 
                    (\(e :: SomeException) -> pure Nothing)
                    (Just <$> readFile f)


readFile'' :: FilePath -> IO (Maybe ByteString)
readFile'' f = (\ma -> case ma of
                        Left _  -> Nothing
                        Right a -> a
                ) <$> readFile' f

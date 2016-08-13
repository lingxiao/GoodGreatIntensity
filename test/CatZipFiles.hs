{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Test output of cat zip files
-- | Author  : Xiao Ling
-- | Date    : 8/11/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
 

module CatZipFiles where


import Core
import Utils
import Play


{-----------------------------------------------------------------------------
    I. Tests
------------------------------------------------------------------------------}

path2  = "/Users/lingxiao/Documents/NLP/Code/Papers/output/2gms.txt"
path2' = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/2gms"

-- * Count the number of lines of a file
countLines :: IO ()
countLines = do
    fs <- readFile path2
    print . length $ fs

countLines2 :: IO ()
countLines2 = do 
    mfs <- seePaths path2'
    case mfs of
        Left _   -> return ()
        Right fs -> do
            let fs' = ((++) path2') <$> fs
            print fs'


{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Play. to be deleted
-- | Author  : Xiao Ling
-- | Date    : 8/10/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
 

module Play where


import Prelude hiding   (readFile, writeFile, concat)

import Control.Monad
import Codec.Compression.GZip     (decompress)
import Control.Exception.Base     (Exception, SomeException,)
import Data.ByteString.Lazy.Char8 (ByteString, readFile, writeFile, concat, pack)
import System.Directory           (getDirectoryContents, doesDirectoryExist)

import Core
import Utils


-- * Notes: all data in utf8
-- * 

-- * The first problem is what to do with this n gram corpus
-- * download it
-- * figure out its format
-- * what kind of basic counting can you do on it?

-- * Input and output paths
inPath, outPath :: FilePath
inPath  = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/"
outPath = "/Users/lingxiao/Documents/NLP/Code/Papers/GoodGreatIntensity/"

[in2 , in3 , in4 , in5 ] = (\n -> inPath ++ show n ++ "gms/") <$> [2..5]
[out2, out3, out4, out5] = (\n -> show n ++ "gms.txt")        <$> [2..5]


-- * Given path to input directory `inPath` and output directory 'outPath'
-- * find and unzip all .gz files and concat into one giant file,
-- * then save at `outPath` with `outName`
concatGzFiles :: FilePath -> FilePath -> String -> IO (Message String)
concatGzFiles inPath outPath outName = do
    mfs <- pathErr inPath outPath outName
    case mfs of
        Left e   -> return e
        Right fs -> do
            glue (outPath ++ outName) $ ((++) inPath) <$> fs 
            return Success

-- * concat all the files found in `fs` together and save to `outpath`
glue :: FilePath -> [FilePath] -> IO ()   
glue outPath fs = do
    bs <- foldM concatFile mempty fs
    writeFile outPath bs


-- * Open file from path `f` and
-- * if file exist, concat file with `bbs`
-- * if file does not exist, then output `bbs` unchanged
-- * Note: this function fails silently
concatFile :: ByteString -> FilePath -> IO ByteString
concatFile bbs f = do
    mbs <- (fmap . fmap) decompress $ readFile' f
    case mbs of
        Left _   -> return bbs
        Right bs -> return . concat $ [bs, bbs]


-- * Check if : `inPath` exist
-- *            `outPath` exist
-- *            `outName` is valid
pathErr :: FilePath 
            -> FilePath 
            -> String 
            -> IO (Either (Message String) [FilePath])

pathErr inPath outPath outName 
    | null outName  = return . Left . Other $ "output name invalid"
    | otherwise     = do
        yes <- doesDirectoryExist outPath
        case yes of
            False -> return . Left . FE . NonExistantOutDir $ outPath
            _     -> do
                mfs <- getDirContents inPath
                case mfs of
                    Left e   -> return . Left . FE $ e
                    Right fs -> return . return $ fs




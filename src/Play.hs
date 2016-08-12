{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Play. to be deleted
-- | Author  : Xiao Ling
-- | Date    : 8/10/2016
-- | Note    : check swaped file usage:   sysctl vm.swapusage
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
 

module Play where


import Prelude hiding             (writeFile, concat)

import Control.Monad
import Control.Monad.State        (StateT, modify, liftIO, runStateT)
import System.Directory           (getDirectoryContents, doesDirectoryExist)
import Data.ByteString.Lazy.Char8 (ByteString, writeFile, concat)
import Codec.Compression.GZip     (decompress)

import Core
import Utils


-- * Input and output paths
inPath, outPath :: FilePath
inPath  = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/"
outPath = "/Users/lingxiao/Documents/NLP/Code/Papers/output/"

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
-- * NOTE: FoldM is stric so this puts the whole file in memory!
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
        Right bs -> do
            print "------------ Concactenating file -----------"
            return . concat $ [bs, bbs]


-- * Check if : `inPath` exist
-- *            `outPath` exist
-- *            `outName` is valid
-- * if yes then output all file paths in directory `inPath`
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
            _     -> seePaths inPath

------------------------------------------------
------------------------------------------------

inP  = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/toyset/"
outP = "/Users/lingxiao/Documents/NLP/Code/Papers/GoodGreatIntensity/"

file = in2 ++ "2gm-0014.gz"


--glue' :: FilePath -> [FilePath] -> IO ()   
--glue' outPath fs = do
--    bs <- foldM concatFile mempty fs
--    writeFile outPath bs


catFile :: ByteString -> FilePath -> StateT Int IO ByteString
catFile bbs f = do
    mbs <- liftIO $ readFile' f 
    case mbs of
        Left _   -> return bbs
        Right bs -> modify (+1) >> (return . concat $ [bs, bbs])


m = catFile mempty file



    --mbs <- (fmap . fmap) decompress $ readFile' f

    --case mbs of
    --    Left _   -> return bbs
    --    Right bs -> do
    --        return . concat $ [bs, bbs]











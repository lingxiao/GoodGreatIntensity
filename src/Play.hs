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


import System.Directory (getDirectoryContents)
import Prelude hiding   (readFile, writeFile, concat)

import Codec.Compression.GZip     (decompress)
import Control.Exception.Base     (Exception, SomeException,)
import Data.ByteString.Lazy.Char8 (ByteString, readFile, writeFile, concat, pack)

import Core
import Utils


-- * Notes: all data in utf8
-- * 

-- * The first problem is what to do with this n gram corpus
-- * download it
-- * figure out its format
-- * what kind of basic counting can you do on it?


-- * start by playing with google one gram

path1, path2 :: FilePath

path  = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/2gms/"

path1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/2gms/2gm-0015.gz"
path2 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/2gms/2gm-0016.gz"
out   = "/Users/lingxiao/Documents/NLP/Code/Papers/GoodGreatIntensity/src/foo.txt"

bar :: IO ()
bar = do
    fs <- getDirectoryContents path
    print fs


concatFile :: FilePath -> ByteString -> IO ByteString
concatFile f bbs = do
    bs <- decompress <$> readFile f
    return . concat $ [bs, bbs]


foo :: IO ()
foo = do
    xs1 <- decompress <$> readFile path1  
    xs2 <- decompress <$> readFile path2 
    let xs = concat[xs1, xs2]  ::  ByteString
    return ()
    --writeFile out xs


{-----------------------------------------------------------------------------

tryJust (\(e :: SomeException) -> pure Nothing) (Just <$> readFile "foo.txt")

mainNoConduit :: FilePath -> FilePath  -> IO ()
mainNoConduit f1 f2 = do
  xxs  <- readFile f1
  let xxs' = unpack . preprocess . pack $ xxs
  writeFile f2 xxs'
    where preprocess = toCaseFold . strip
------------------------------------------------------------------------------}





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

import Lib  

main :: IO ()
main = print "hello Good Great"




{-----------------------------------------------------------------------------
   Untar files
   These form the first step in a end to end application

untar :: IO ()
untar = do
    run $ untarGz path2
    run $ untarGz path3
    run $ untarGz path4
    run $ untarGz path5

untarGz :: FileOpS m s => FilePath -> m ()
untarGz p = untarAll p ".gz" ".txt"

path2, path3, path4, path5 :: FilePath
path2 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/2gms/"
path3 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/3gms/"
path4 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/4gms/"
path5 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/5gms/"


pip1 :: FileOpS m s => m Int
pip1 =  runConduit $ path2 `traverseAll` ".txt"
    =$= mapMC (\p -> liftIO $ fmap (length . B.lines) $ B.readFile p)
    =$= foldlC (+) 0


-- * Count the number of lines in file at path `p`
-- * If file path invalid, output 0 for empty file
countLine :: FileOpS m s => FilePath -> m ()
countLine p =   runConduit $ sourceFileE p 
            =$= numLines =$= logi =$= cap

            
------------------------------------------------------------------------------}

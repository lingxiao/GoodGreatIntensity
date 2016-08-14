{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Counting occurences of items in data
-- | Author  : Xiao Ling
-- | Date    : 8/12/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module CountNgram where

import Data.Conduit
import Data.Conduit.Filesystem      (sourceDirectory     )
import Conduit hiding               (sourceDirectory     ,
                                     sourceFile          )


import Core
import ConduitLib

{-----------------------------------------------------------------------------
   Untar files
------------------------------------------------------------------------------}

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


{-----------------------------------------------------------------------------
   Samples

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


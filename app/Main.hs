{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
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


import System.Directory
import Data.Text hiding (foldr)
import Control.Monad.Trans.Reader

import Core
import Parsers
import Patterns
import Conduits
import Preprocess


import Score
import Patterns

{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}


main :: IO ()
main = do
    createDirectoryIfMissing False "small_minute"
    runReaderT (cntwd $ word "small" ) sys_small_minute'
    runReaderT (cntwd $ word "minute") sys_small_minute'
    return ()

    --createDirectoryIfMissing False "good_great"
    --runReaderT (w1 "good" "great") sys
    --return ()

{-----------------------------------------------------------------------------
  System Paths 
------------------------------------------------------------------------------}

sys_good_great    = sys "good_great"
sys_good_great'   = sysr "good_great"
sys_small_minute  = sys "small_minute"
sys_small_minute' = sysr "small_minute"

sys :: String -> Sys
sys xs = S xs f1 [f4,f5]

sysr :: String -> Sys
sysr xs = S xs f1r [f4r,f5r]

{-----------------------------------------------------------------------------
  FilePath
------------------------------------------------------------------------------}

-- * remote
f1r = "/nlp/data/xiao/ngram/data/1gms"
f2r = "/nlp/data/xiao/ngram/data/2gms"
f3r = "/nlp/data/xiao/ngram/data/3gms"
f4r = "/nlp/data/xiao/ngram/data/4gms"
f5r = "/nlp/data/xiao/ngram/data/5gms"

-- * local
fd = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/dummydata"
f1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/1gms"
f2 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/2gms"
f3 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/3gms"
f4 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/4gms"
f5 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/5gms"





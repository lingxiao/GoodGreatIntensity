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
import MainSubroutines


main :: IO ()
main = do
    main_p1 con_remote
    main_p2 con_remote


{-----------------------------------------------------------------------------
  System Paths
------------------------------------------------------------------------------}

con_local :: Config
con_local = Con f1 [fd] fsw fws


con_remote :: Config
con_remote = Con f1r [f4r, f5r] fswr fwsr


-- * local ngram directory
fd = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/dummydata"
f1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/1gms"
f4 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/4gms"
f5 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/5gms"
-- * local pattern directory
fsw = "/Users/lingxiao/Documents/NLP/Code/GoodGreatIntensity/configs/strong-weak-patterns.txt"
fws = "/Users/lingxiao/Documents/NLP/Code/GoodGreatIntensity/configs/weak-strong-patterns.txt"

-- * remote ngram directory
f1r, f4r, f5r :: FilePath
f1r = "/nlp/data/xiao/ngrams/1gms"
f4r = "/nlp/data/xiao/ngrams/4gms"
f5r = "/nlp/data/xiao/ngrams/5gms"
-- * remote pattern path
fswr = "/home1/l/lingxiao/xiao/GoodGreatIntensity/configs/strong-weak-patterns.txt"
fwsr = "/home1/l/lingxiao/xiao/GoodGreatIntensity/configs/weak-strong-patterns.txt"




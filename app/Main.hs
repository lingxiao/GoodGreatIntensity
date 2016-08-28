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

sys, sysr :: Sys
sys  = S "good_great" f1  [f4,f5]
sysr = S "good_great" f2r [f4r,f5r]

main :: IO ()
main = do
    createDirectoryIfMissing False "good_great"
    runReaderT (w1 "good" "great") sys
    return ()

{-----------------------------------------------------------------------------
  Paths
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





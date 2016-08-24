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


import Data.Text hiding (foldr)

import Core
import Parsers
import Patterns
import Conduits
import Preprocess
import Count

{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}

main :: IO ()
main = do
    (n, xs) <- eval $ cnt (word "good") p1r
    writeResult "good.txt" n xs


{-----------------------------------------------------------------------------
  Paths
------------------------------------------------------------------------------}

-- * remote
p1r = "/nlp/data/xiao/Ngrams/data/1gms"


-- * local
p  = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/5gmsG"
p5 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/5gms"
p4 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/4gms"
p1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/1gms"
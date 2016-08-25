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
     (n, xs) <- eval . cnt p1 $ word "great"
     writeResult "great.txt" n xs

    --(n, xs) <- eval . cnt ps $ "good" `butNot` "great"
    --writeResult "good_but_not_great.txt" n xs


{-----------------------------------------------------------------------------
  Paths
------------------------------------------------------------------------------}

-- * remote
p1r = "/nlp/data/xiao/Ngrams/data/1gms"


-- * local
ps = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/5gmsG"
p5 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/5gms"
p4 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/4gms"
p1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/1gms"
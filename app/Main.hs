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
    pws1 p5


{-----------------------------------------------------------------------------
  queries
------------------------------------------------------------------------------}

-- * weak strong pattern 1
pws1 :: FilePath -> IO ()
pws1 f = do
    let p = cnt ("good" `butNot` "great")
    (n, xs) <- eval (p f) []
    writeResult "goodButNotGreat.txt" n xs 



p = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/5gmsG"
p5 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/5gms"
p4 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/4gms"
p1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/1gms"
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

import Untar
import Score
import Patterns

{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}

main :: IO ()
main = do
    cnt p4 "good" "great" (p_weakStrong !! 2)
    cnt p5 "good" "great" (p_weakStrong !! 2)

    cnt p4 "great" "good" (p_weakStrong !! 2)
    cnt p5 "great" "good" (p_weakStrong !! 2)



{-----------------------------------------------------------------------------
  routines
------------------------------------------------------------------------------}

untar :: IO ()
untar = do
    run $ untarAll p1 ".gz" ".txt"
    run $ untarAll p2 ".gz" ".txt"
    run $ untarAll p3 ".gz" ".txt"
    run $ untarAll p4 ".gz" ".txt"
    run $ untarAll p5 ".gz" ".txt"

{-----------------------------------------------------------------------------
  Paths
------------------------------------------------------------------------------}

-- * remote
p1r = "/nlp/data/xiao/ngram/data/1gms"
p2r = "/nlp/data/xiao/ngram/data/2gms"
p3r = "/nlp/data/xiao/ngram/data/3gms"
p4r = "/nlp/data/xiao/ngram/data/4gms"
p5r = "/nlp/data/xiao/ngram/data/5gms"

-- * local
p1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/1gms"
p2 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/2gms"
p3 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/3gms"
p4 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/4gms"
p5 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/5gms"





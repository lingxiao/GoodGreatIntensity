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
main = undefined

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
f1r = "/nlp/data/xiao/ngram/data/1gms"
f2r = "/nlp/data/xiao/ngram/data/2gms"
f3r = "/nlp/data/xiao/ngram/data/3gms"
f4r = "/nlp/data/xiao/ngram/data/4gms"
f5r = "/nlp/data/xiao/ngram/data/5gms"

-- * local
f1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/1gms"
f2 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/2gms"
f3 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/3gms"
f4 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/4gms"
f5 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/5gms"





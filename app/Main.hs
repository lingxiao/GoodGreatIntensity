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
import Count

{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}

main :: IO ()
main = undefined


-- * given weak word `good` and strong word `great`
-- * query all counts and save to this directory
query :: String -> String -> IO ()
query good great = do
    (n, xs) <- eval . cnt p1 $ word good
    writeResult (good ++ ".txt") n xs

    (n, xs) <- eval . cnt p1 $ word great
    writeResult (great ++ ".txt") n xs

    -- * weak-strong patterns
    (n, xs) <- eval . cnt p4 $ good `butNot` great
    writeResult (good ++ "_but_not_" ++ great ++ "_4gms.txt") n xs




    -- * strong-weak patterns


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





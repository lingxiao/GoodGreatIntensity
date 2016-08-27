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
main = do
    -- * untar all files
    run $ untarAll p1r ".gz" ".txt"
    run $ untarAll p2r ".gz" ".txt"
    run $ untarAll p3r ".gz" ".txt"
    --run $ untarAll p4 ".gz" ".txt"
    --run $ untarAll p5 ".gz" ".txt"

    -- * count all occurences and save output to this directory




    --(n, xs) <- eval . cnt p1 $ word "great"
    --writeResult "great.txt" n xs

    --(n, xs) <- eval . cnt ps $ "good" `butNot` "great"
    --writeResult "good_but_not_great.txt" n xs

{-----------------------------------------------------------------------------
  Paths
------------------------------------------------------------------------------}

-- * remote
p1r = "/nlp/data/xiao/LDC2006T13/data/1gms"
p2r = "/nlp/data/xiao/LDC2006T13/data/2gms"
p3r = "/nlp/data/xiao/LDC2006T13/data/3gms"
p4r = "/nlp/data/xiao/LDC2006T13/data/4gms"
p5r = "/nlp/data/xiao/LDC2006T13/data/5gms"

-- * local
p1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/1gms"
p2 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/2gms"
p3 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/3gms"
p4 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/4gms"
p5 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/5gms"





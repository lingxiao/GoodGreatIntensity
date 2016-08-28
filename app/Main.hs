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

sys :: Sys
sys = S "good_great" f1 [f4,f5]

main :: IO ()
main = do
    createDirectory "good_great"
    runReaderT (score "good" "great") sys
    return ()

{-----------------------------------------------------------------------------
  routines
------------------------------------------------------------------------------}

--untar :: IO ()
--untar = do
--    run $ untarAll p1 ".gz" ".txt"
--    run $ untarAll p2 ".gz" ".txt"
--    run $ untarAll p3 ".gz" ".txt"
--    run $ untarAll p4 ".gz" ".txt"
--    run $ untarAll p5 ".gz" ".txt"

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





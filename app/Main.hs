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


main :: IO ()
main = do
    mapM countw testwords
    return ()

    --createDirectoryIfMissing False "good_great"
    --runReaderT (w1 "good" "great") sys
    --return ()

{-----------------------------------------------------------------------------
  words
------------------------------------------------------------------------------}

countw :: String -> IO Integer
countw a = runReaderT (cntwd . word $ a) sys_words

testwords = [ "better"
            , "best"
            , "acceptable"
            , "satisfactory"
            , "good"
            , "great"
            , "solid"
            , "superb"
            , "muddy"
            , "sticky"
            , "humid"
            , "tacky"
            , "moist"
            , "damp"
            , "steamy"
            , "wet"
            , "drippy"
            , "watery"
            , "boggy"
            , "soggy"
            , "rainy"
            , "waterlogged"
            , "evil"
            , "negative"
            , "mediocre"
            , "poor"
            , "bad"
            , "worse"
            , "awful"
            , "worst"
            , "terrible"
            , "innocent"
            , "simple"
            , "naive"
            , "childlike"
            , "limited"
            , "special"
            , "peculiar"
            , "specific"
            , "particular"
            , "unique"
            ]


{-----------------------------------------------------------------------------
  System Paths 
------------------------------------------------------------------------------}

sys_words :: Sys
sys_words = S "words" f1 []

sys :: String -> Sys
sys xs = S xs f1 [f4,f5]

sysr :: String -> Sys
sysr xs = S xs f1r [f4r,f5r]

{-----------------------------------------------------------------------------
  FilePath
------------------------------------------------------------------------------}

-- * remote
f1r = "/nlp/data/xiao/ngrams/1gms"
f2r = "/nlp/data/xiao/ngrams/2gms"
f3r = "/nlp/data/xiao/ngrams/3gms"
f4r = "/nlp/data/xiao/ngrams/4gms"
f5r = "/nlp/data/xiao/ngrams/5gms"

-- * local
fd = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/dummydata"
f1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/1gms"
f2 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/2gms"
f3 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/3gms"
f4 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/4gms"
f5 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/5gms"





{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Run these routines in main on nlpgrid
-- | Author  : Xiao Ling
-- | Date    : 8/10/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
 
module CountRoutines (
        countWords
      , countStar
      ) where


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
  Routines
------------------------------------------------------------------------------}

-- * count training set
countWords :: IO ()
countWords = do
      createDirectoryIfMissing False "words"
      mapM go testwords >> return ()
            where go a = runReaderT (cntwd . word $ a) sys


-- * P1 and P2
countStar :: IO ()
countStar = runReaderT go sys >> return ()
      where go = do
            sumcnt $ p_weakStrong star star
            sumcnt $ p_strongWeak star star


{-----------------------------------------------------------------------------
  words
------------------------------------------------------------------------------}

testwords = [ "good"
            , "bad"
            , "better"
            , "best"
            , "acceptable"
            , "satisfactory"
            , "good"
            , "great"
            , "solid"
            , "superb"

            , "wet"
            , "dry"
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

            , "sophisticated"
            , "naif"
            , "innocent"
            , "simple"
            , "naive"
            , "childlike"

            , "characteristic"
            , "uncharacteristic"
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

sys :: Sys
sys = S "words" f1r [f4r,f5r]

-- * remote
f1r, f4r, f5r :: FilePath
f1r = "/nlp/data/xiao/ngrams/1gms"
f4r = "/nlp/data/xiao/ngrams/4gms"
f5r = "/nlp/data/xiao/ngrams/5gms"


-- * local
fd = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/dummydata"
f1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/1gms"
f2 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/2gms"
f3 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/3gms"
f4 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/4gms"
f5 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/5gms"




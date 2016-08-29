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
 
module CountWords (
        countWords
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
  Main
------------------------------------------------------------------------------}

countWords :: IO ()
countWords = mapM countw testwords >> return ()


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

sys_words' :: Sys
sys_words' = S "words" f1r []


-- * remote
f1r = "/nlp/data/xiao/ngrams/1gms"

-- * local
f1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/1gms"





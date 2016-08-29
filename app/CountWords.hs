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
countWords = do
      createDirectoryIfMissing False "words"
      mapM go testwords >> return ()

{-----------------------------------------------------------------------------
  words
------------------------------------------------------------------------------}

go :: String -> IO Integer
go a = runReaderT (cntwd . word $ a) sys_words

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

sys_words :: Sys
sys_words = S "words" f1r []

f1r :: FilePath
f1r = "/nlp/data/xiao/ngrams/1gms"






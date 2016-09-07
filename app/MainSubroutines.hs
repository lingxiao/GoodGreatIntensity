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
 
module MainSubroutines (
        main_words
      , main_weakStrong
      , main_strongWeak
      ) where


import System.Directory
import System.FilePath.Posix
import Control.Monad.Trans.Reader

import Data.List.Split  (splitOn)
import Data.Text hiding (foldr, concat, splitOn)

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

-- * count frequency of all words. from paper: cnt(ai)
main_words :: IO [Integer]
main_words = do
      makeDir "words"
      mapM go twords
            where go a = runReaderT (cntwd . word $ a) 
                       $ S "words" f1r []

-- * P1 from paper: Σ_{p_i ∈ Pws} cnt(p_i)
main_weakStrong :: IO Integer
main_weakStrong = do
      makeDir "P1"
      runReaderT go $ S "P1" f1r [f4r,f5r]
            where go = sumcnt $ p_weakStrong star star

-- * P2 from paper: Σ_{p_i ∈ Psw} cnt(p_i)
main_strongWeak :: IO Integer
main_strongWeak = do
      makeDir "P2"
      runReaderT go $ S "P2" f1r [f4r,f5r]
            where go = sumcnt $ p_strongWeak star star

sGoodbad = 
      let ws = [(u,v) | u <- goodbad, v <- goodbad, u /= v]
      in ws


makeDir :: FilePath -> IO FilePath
makeDir p = do
      xs <- getCurrentDirectory
      let project = "GoodGreatIntensity"
      let top:_   = splitOn project xs
      let dir     = top ++ project ++ "/" ++ takeBaseName p
      createDirectoryIfMissing False dir
      return dir




{-----------------------------------------------------------------------------
  words
------------------------------------------------------------------------------}

-- * TODO: move these into a .txt file
-- * TODO: a sane output input directory structure
twords = concat [goodbad, wetdry, negmediocre, sophNaif, character]

goodbad =   [ "good"
            , "bad"
            , "better"
            , "best"
            , "acceptable"
            , "satisfactory"
            , "good"
            , "great"
            , "solid"
            , "superb"]

wetdry =    [ "wet"
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
            , "waterlogged"]

negmediocre = ["evil"
            , "negative"
            , "mediocre"
            , "poor"
            , "bad"
            , "worse"
            , "awful"
            , "worst"
            , "terrible"]

sophNaif  = ["sophisticated"
            , "naif"
            , "innocent"
            , "simple"
            , "naive"
            , "childlike"]

character = ["characteristic"
            , "uncharacteristic"
            , "limited"
            , "special"
            , "peculiar"
            , "specific"
            , "particular"
            , "unique"]

{-----------------------------------------------------------------------------
  System Paths 
------------------------------------------------------------------------------}

sys :: Sys
sys = S "Ps" f1r [fd]

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





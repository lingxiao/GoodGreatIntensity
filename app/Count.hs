{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Collect statistics
-- | Author  : Xiao Ling
-- | Date    : 8/17/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Count where

import Prelude hiding (filter)

import Control.Monad.IO.Class 

import Data.Text
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator

import Data.Conduit 
import Data.Conduit.Text
import Data.Conduit.List
import Conduit (mapC, scanlC, filterC)



import Core
import Parsers
import Conduits
import Preprocess


main :: IO ()
main = run bar

p  = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/5gmsG/"
p5 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/5gms/"

bar :: FileOp m => m ()
bar =   runConduit 
    $   streamLines p5
    =$= logi
    =$= countOccur ("good" `butNot` "great")
    =$= logi
    =$= cap


-- * open all ".txt" files found at path `p` and stream them as lines
-- * preprocess each line 
streamLines :: FileOp m => FilePath -> Source m (Text, Int)
streamLines p =  p `traverseAll` ".txt"
             =$= openFile
             =$= linesOn "\t"
             =$= filterC (\x -> Prelude.length x == 2)
             =$= mapC    (\[w,n] -> (pre w, read . unpack $ n :: Int))
       
-- * search for pattern `p` and sum all of its occurences
countOccur :: FileOp m => Parser Text -> Conduit (Text, Int) m Int
countOccur p =  filterC (\(w,_) -> if p <** w == Nothing then False else True)
            =$= logi
            =$= scanlC (\m (w,n) -> m + n) 0













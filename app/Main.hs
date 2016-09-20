-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Main
-- | Author  : Xiao Ling
-- | Date    : 9/11/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Main where


import Data.Text hiding (head, replicate, filter, foldr, zip)
import System.FilePath.Posix


import Src
import Lib
import Scripts

{-----------------------------------------------------------------------------
  Main
------------------------------------------------------------------------------}

main :: IO ()
main = do
  (main_normalize l4gm "4gm") `mapM` [30..131]
  return ()


{-----------------------------------------------------------------------------
    Configurations
------------------------------------------------------------------------------}

-- * remote
r4gm, r4gm_scrub, r5gm, r5gm_scrub :: DirectoryPath
r4gm       = "/nlp/data/xiao/ngrams/raw/4gms/"
r4gm_scrub = r4gm ++ "scrub/"
r5gm       = "/nlp/data/xiao/ngrams/raw/5gms/"
r5gm_scrub = r5gm ++ "scrub/"



-- * local
l4gm = "/Users/lingxiao/Documents/research/data/ngrams/raw/4gms/"



grep_sm = "/Users/lingxiao/Documents/research/data/ngrams/grep-small/"
grep_ws = "/Users/lingxiao/Documents/research/data/ngrams/grep-weak-strong/"
grep_sw = "/Users/lingxiao/Documents/research/data/ngrams/grep-strong-weak/"


corpus_l   = "/Users/lingxiao/Documents/research/data/ngrams/corpus/"
patterns_l = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/"

root_ws    = "/Users/lingxiao/Documents/research/data/ngrams/grep-weak-strong/"
root_sw    = "/Users/lingxiao/Documents/research/data/ngrams/grep-strong-weak/"


config_l :: IO Config
config_l = do
    Just con <- config corpus_l patterns_l
    return con

























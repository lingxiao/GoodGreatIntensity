{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Read and write file to system
-- | Author  : Xiao Ling
-- | Date    : 9/7/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module System where

import Prelude hiding (filter)

import System.Directory
import System.FilePath.Posix
import qualified System.IO as S

import Control.Monad.State  
import Control.Monad.IO.Class 
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader

import Data.List.Split             (splitOn)
import Data.Conduit 
import Data.Text            hiding (foldr, length, count, splitOn)
import Data.Attoparsec.Text hiding (count)
import Conduit              (mapC, scanlC, foldlC, filterC)

import Core
import Parsers
import Patterns
import Conduits
import Preprocess


{-----------------------------------------------------------------------------
  Types

thoughts:

you need an actual config with:

path to inputs
path to outputs

functions working with config so that you can save at anytime

all code should be file structure agnostic

all print messages should appear here

look at the .cabal file for inspirations


assets:
  path to ngrams
  path to saved files
  path to words to be looked up

need to load .config file and put into data structure:

.config:

n-grams:

../path1  
../path2 
../path3  
../path4  
../path5  

output:
/path/to/output

------------------------------------------------------------------------------}

type ParseResult = (Text,Text,Integer)
type OutPath     = FilePath
data Sys         = S { out :: FilePath, onegram :: FilePath, ngrams :: [FilePath]}
                       deriving (Show)


{-----------------------------------------------------------------------------
  Read
------------------------------------------------------------------------------}


{-----------------------------------------------------------------------------
  Write
------------------------------------------------------------------------------}



{-----------------------------------------------------------------------------
  Paths
------------------------------------------------------------------------------}

-- * given a parser `p`,
-- * `getFile`s in `sys`tem
-- * `query` each file, print results, and `save` results
-- * int `out`put directory specified by `sys`tem
querySave :: Parser Text 
          -> (Sys -> [FilePath]) 
          -> ReaderT Sys IO Integer
querySave parser getFile = do
  sys     <- ask
  let (outp, fs) = (out sys, getFile sys)
  (n, ts) <- query fs parser
  liftIO $ writeResult (outp ++ "/" ++ name parser) n ts

  liftIO $ print $ name parser ++ "  " ++ show n
  liftIO $ print "================================="
  liftIO $ mapM (\(w,w',n) ->  print
                  $  unpack w 
                  ++ "     " 
                  ++ unpack w' 
                  ++ "     "
                  ++ show n
                  ++ "\n") ts
    
  return $ fromInteger n


-- * write result named `name` to local directory,
-- * result is total count `n` and incidences occured `xs`
writeResult :: String 
            -> Integer
            -> [ParseResult] 
            -> IO ()
writeResult xs n ts = do
    let ys   = dropExtension xs
    let file = ys ++ ".txt"
    let name = takeFileName file
    o <- S.openFile file S.WriteMode
    S.hPutStrLn o name
    S.hPutStrLn o mark
    S.hPutStrLn o $ "total: " ++ show n
    S.hPutStrLn o mark
    mapM (\(w,w',n) ->  S.hPutStrLn o 
                     $  unpack w 
                     ++ "     " 
                     ++ unpack w' 
                     ++ "     "
                     ++ show n) ts
    S.hClose o
    return ()
        where mark = foldr (++) mempty $ (const "-") <$> [1..50] 


-- * write result named `name` to this directory,
-- * result is score equation `eq` and actual score `n`
writeScore :: Show a => FilePath -> String -> a -> IO ()
writeScore xs eq n = do
    let ys   = dropExtension xs
    let file = ys ++ ".txt"
    let name = takeFileName file
    o <- S.openFile file S.WriteMode
    S.hPutStrLn o name
    S.hPutStrLn o mark
    S.hPutStrLn o $ "score: " ++ show n
    S.hPutStrLn o mark
    S.hPutStrLn o eq
    S.hClose o
    return ()
        where mark = foldr (++) mempty $ (const "-") <$> [1..50] 


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






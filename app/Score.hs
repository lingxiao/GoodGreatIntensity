{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : All functions used to find score
-- | Author  : Xiao Ling
-- | Date    : 8/17/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Score where

import Prelude hiding (filter)

import System.Directory
import qualified System.IO as S
import System.FilePath.Posix
import Control.Monad.State  
import Control.Monad.IO.Class 
import Control.Monad.Trans.Resource

import Data.Conduit 
import Conduit (mapC, scanlC, foldlC, filterC)
import Data.Text hiding (foldr, length)
import Data.Attoparsec.Text

import Core
import Parsers
import Patterns
import Conduits
import Preprocess

{-----------------------------------------------------------------------------
  Type
------------------------------------------------------------------------------}

type ParseResult = (Text,Text,Int)
type Total       = Int


{-----------------------------------------------------------------------------
  Score 
------------------------------------------------------------------------------}

f1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/1gms"
f = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/dummydata"

score :: Adjective -> Adjective -> FilePath -> [FilePath] -> IO Int
score a1 a2 f fs = do

  -- * create directory


  -- * compute score
  p1' <- p1 fs
  p2' <- p2 fs

  w1' <- w1 a1 a2 fs
  w2' <- w2 a2 a2 fs
  
  s1' <- s1 a1 a2 fs
  s2' <- s2 a1 a2 fs

  a1' <- cnt f (a1, word a1)
  a2' <- cnt f (a2, word a1)

  let w1'' = quot w1' p1'
  let w2'' = quot w2' p1'
  let s1'' = quot s1' p2'
  let s2'' = quot s2' p2'

  let top = (w1'' - s1'') - (w2'' - s2')
  let bot = a1' * a2'

  let score' = quot top bot

  -- * save file
  let xs1 = "(" ++ show w1'' ++ " - " ++ show s1'' ++ ")"
  let xs2 = "(" ++ show w2'' ++ " - " ++ show s2'' ++ ")"
  let xs3 = show a1' ++ " * " ++ show a2'
  let len = length xs3 + 10

  let eq = show score' ++ " = "
          ++ xs1 ++ " - " ++ xs2 ++ "  /  " ++ xs3

  let name = a1 ++ "_" ++ a2 ++ ".txt"

  writeScore name eq score'

  return score'


{-----------------------------------------------------------------------------
  Score parts
------------------------------------------------------------------------------}


-- * unormalized value `w1`
w1 :: Adjective -> Adjective -> [FilePath] -> IO Total
w1 a1 a2 = sumcnt (p_weakStrong a1 a2)

w2 :: Adjective -> Adjective -> [FilePath] -> IO Total
w2 a1 a2 = w1 a2 a1

s1 :: Adjective -> Adjective -> [FilePath] -> IO Total
s1 a1 a2 = sumcnt (p_strongWeak a1 a2)

s2 :: Adjective -> Adjective -> [FilePath] -> IO Total
s2 a1 a2 = s1 a2 a1

p1 :: [FilePath] -> IO Total
p1 = sumcnt (p_weakStrong "*" "*")

p2 :: [FilePath] -> IO Total
p2 = sumcnt (p_strongWeak "*" "*")


{-----------------------------------------------------------------------------
  Score subroutines
------------------------------------------------------------------------------}

sumcnt :: (Name, [Pattern])
      -> [FilePath] 
      -> IO Total
sumcnt (name, ps) fs = do
  ns <- mapM (\p -> mapM (\f -> cnt f p) fs) ps
  let m = sum . join $ ns
  writeResult (name ++ ".txt") m []
  return m

cnt :: FilePath -> Pattern -> IO Total
cnt f (name, p) = do
  (n,xs) <- query f p
  writeResult (name ++ ".txt") n xs
  return n

{-----------------------------------------------------------------------------
  Conduit routines
------------------------------------------------------------------------------}

-- * `query` for occurences of `p` in all files found at path `f`
query :: Op m 
     => FilePath -> Parser Text -> m (Total,[ParseResult])
query f p  = eval $ streamLines f $$ countOccur p


-- * open all ".txt" files found at path `p` and stream them as lines
-- * preprocess each line by casefolding and stripping of whitespace
streamLines :: FileOpS m s => FilePath -> Source m ParseResult
streamLines f =  f `traverseAll` ".txt"
             =$= openFile
             =$= linesOn "\t"
             =$= filterC (\x -> Prelude.length x == 2)
             =$= mapC    (\[w,n] -> (pre w, w, read . unpack $ n :: Int))

       
-- * search for pattern `p` and sum all of its occurences
-- * save occurences in local state
countOccur :: FileOpS m [ParseResult]
           => Parser Text 
           -> Consumer ParseResult m Int
countOccur p =  filterC (\(w,_,_) -> 
                        if p <** w == Nothing then False 
                                              else True)
            =$= logi
            =$= awaitForever (\t -> do
                    ts <- lift get
                    let ts' = t:ts
                    lift . put $ ts'
                    yield t
                )
            =$= foldlC  (\m (_,_,n) -> m + n) 0

{-----------------------------------------------------------------------------
  Save file
------------------------------------------------------------------------------}

-- * write result named `name` to local directory,
-- * result is total count `n` and incidences occured `xs`
writeResult :: String -> Int -> [ParseResult] -> IO ()
writeResult name n ts = do
    o <- S.openFile name S.WriteMode
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
writeScore :: String -> String -> Int -> IO ()
writeScore name eq n = do
    o <- S.openFile name S.WriteMode
    S.hPutStrLn o name
    S.hPutStrLn o mark
    S.hPutStrLn o $ "score: " ++ show n
    S.hPutStrLn o mark
    S.hPutStrLn o eq
    S.hClose o
    return ()
        where mark = foldr (++) mempty $ (const "-") <$> [1..50] 









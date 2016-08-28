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

import qualified System.IO as S
import Control.Monad.State  
import Control.Monad.IO.Class 

import Data.Conduit 
import Conduit (mapC, scanlC, foldlC, filterC)
import Data.Text hiding (foldr)
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

p = "/Users/lingxiao/Documents/NLP/Code/Datasets/ngrams/dummydata"

--w1 :: FilePath -> String -> String -> Pws -> IO ()
--w1 f a1 a2 = mapM (\pattern -> cnt $ a1 `pattern` a2) 


--cnt :: Parser Text -> IO ()
--cnt = undefined

{--

w1 = go 


go f a1 a2 = do
  (name, pattern) <- pws
  (n,xs)          <- cnt' f $ a1 `pattern` a2
  writeResult (name a1 a2) n xs



--}


{-----------------------------------------------------------------------------
  Count occurences of `p` in file at path `f`
------------------------------------------------------------------------------}

--cnt :: FileOpS m [Count] 
    -- => FilePath -> Parser Text -> m Int
cnt' :: FileOpS m [ParseResult] 
    => FilePath -> Parser Text -> m (Total,[ParseResult])
cnt' f p  = eval $ streamLines f $$ countOccur p

{-----------------------------------------------------------------------------
  Conduit routines
------------------------------------------------------------------------------}

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
countOccur p =  filterC (\(w,_,_) -> if p <** w == Nothing then False 
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











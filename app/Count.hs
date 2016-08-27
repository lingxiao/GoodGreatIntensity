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
  Main application function
------------------------------------------------------------------------------}

-- * cnt occurences of pattern `p` in files found in `f`
cnt :: FileOpS m [(Text, Text, Int)] 
    => FilePath -> Parser Text -> m Int
cnt f p  = streamLines f $$ countOccur p

{-----------------------------------------------------------------------------
  Conduit routines
------------------------------------------------------------------------------}

-- * open all ".txt" files found at path `p` and stream them as lines
-- * preprocess each line by casefolding and stripping of whitespace
streamLines :: FileOpS m s => FilePath -> Source m (Text, Text, Int)
streamLines p =  p `traverseAll` ".txt"
             =$= openFile
             =$= linesOn "\t"
             =$= filterC (\x -> Prelude.length x == 2)
             =$= mapC    (\[w,n] -> (pre w, w, read . unpack $ n :: Int))

       
-- * search for pattern `p` and sum all of its occurences
-- * save occurences in local state
countOccur :: FileOpS m [(Text, Text, Int)]
           => Parser Text 
           -> Consumer (Text, Text, Int) m Int
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
  utils
------------------------------------------------------------------------------}

-- * write result named `name` to local directory,
-- * result is total count `n` and incidences occured `xs`
writeResult :: String -> Int -> [(Text,Text,Int)] -> IO ()
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











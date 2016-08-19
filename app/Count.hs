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
   Main
------------------------------------------------------------------------------}

ps = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/5gmsG/"

foo :: IO ()
foo = do
    (n, xs) <- eval pCnt []
    writeResult "goodButNotGreat.txt" n xs

pCnt :: FileOpS m [(Text, Int)] => m Int
pCnt = cnt ps $ "good" `butNot` "great"


{-----------------------------------------------------------------------------
  Conduit routines
------------------------------------------------------------------------------}

cnt :: FileOpS m [(Text, Int)] => FilePath -> Parser Text -> m Int
cnt f p =    runConduit 
         $   streamLines f
         =$= countOccur p


-- * open all ".txt" files found at path `p` and stream them as lines
-- * preprocess each line 
streamLines :: FileOpS m s => FilePath -> Source m (Text, Int)
streamLines p =  p `traverseAll` ".txt"
             =$= openFile
             =$= linesOn "\t"
             =$= filterC (\x -> Prelude.length x == 2)
             =$= mapC    (\[w,n] -> (pre w, read . unpack $ n :: Int))

       
-- * search for pattern `p` and sum all of its occurences
countOccur :: FileOpS m [(Text, Int)]
           => Parser Text 
           -> Consumer (Text, Int) m Int
countOccur p =  filterC (\(w,_) -> if p <** w == Nothing then False else True)
            =$= logi
            =$= awaitForever (\t -> do
                    ts <- lift get
                    let ts' = t:ts
                    lift . put $ ts'
                    yield t
                )
            =$= foldlC  (\m (_,n) -> m + n) 0

{-----------------------------------------------------------------------------
  utils
------------------------------------------------------------------------------}

writeResult :: String -> Int -> [(Text,Int)] -> IO ()
writeResult name n ts = do
    o <- S.openFile name S.WriteMode
    S.hPutStrLn o name
    S.hPutStrLn o mark
    S.hPutStrLn o $ "total: " ++ show n
    S.hPutStrLn o mark
    mapM (\(w,n) -> S.hPutStrLn o $ unpack w ++ show n) ts
    S.hClose o
    return ()
        where mark = foldr (++) mempty $ (const "-") <$> [1..50] 











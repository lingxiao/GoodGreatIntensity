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

module Score (
      count     -- * todo: this function is easily abused
    , w1
    , w2
    , s1
    , s2
    , p1
    , p2
  ) where

import qualified System.IO as S

import Control.Monad.State  
import Control.Monad.Trans.Reader

import Conduit              (mapC, scanlC, foldlC, filterC)

import Data.Conduit 
import Data.List.Split      (splitOn)
import Data.Text            (Text, unpack)
import Data.Attoparsec.Text hiding (count)


import Core
import Conduits
import Preprocess
import PatternCompiler

{-----------------------------------------------------------------------------
  Score 
------------------------------------------------------------------------------}

w1 :: String -> String -> ReaderT Config IO Output
w1 a1 a2 = do
  p_ws <- pattern weakStrong
  sumCount $ (\p -> p a1 a2) <$> p_ws

s1 :: String -> String -> ReaderT Config IO Output
s1 a1 a2 = do
  p_sw <- pattern strongWeak
  sumCount $ (\p -> p a1 a2) <$> p_sw

w2 :: String -> String -> ReaderT Config IO Output
w2 = flip w1

s2 :: String -> String -> ReaderT Config IO Output
s2 = flip s1

p1 :: ReaderT Config IO Output
p1 = w1 "_*_" "_*_"

p2 :: ReaderT Config IO Output
p2 = s1 "_*_" "_*_"

-- * `get` pattern path and open, then compile
pattern :: (Config -> FilePath) -> ReaderT Config IO [Pattern]
pattern get = do
  con <- ask
  ps  <- liftIO $ S.readFile (get con) 
  let ps' = lines ps
  return $ compile <$> ps'

{-----------------------------------------------------------------------------
  Count
------------------------------------------------------------------------------}

-- * sum the results of multiple `count`s 
-- * and sum their counts, list all results
sumCount :: [Parser Text] -> ReaderT Config IO Output
sumCount ps = do
  rrs <- mapM countp ps
  let ns = fst <$> rrs
  let rs = snd <$> rrs
  return (sum ns, concat rs)

-- * `count` for occurences of some phrase among ngram files
countp :: Parser Text -> ReaderT Config IO Output
countp phrase = do
  con     <- ask
  (n, ts) <- (ngrams con) `query` phrase
  return (n,ts)

-- * `count` occurences of some word `w` 
-- * in onegram file
count :: String -> ReaderT Config IO Output
count w = do
  let word = compile w "" ""
  con     <- ask
  (n, ts) <- [onegram con] `query` word
  return (n,ts)

{-----------------------------------------------------------------------------
  Conduit subroutines
------------------------------------------------------------------------------}

-- * `query` for occurences of utterance to be parsed by parser `p` 
-- * in all files found at paths `fs`
query :: (Op m , Fractional a)
      => [FilePath] -> Parser Text -> m Output
query fs p  = eval $ openFiles fs $$ queryFiles p

-- * open all ".txt" files found at paths `fs` and stream them as lines
-- * preprocess each line by casefolding and stripping of whitespace
openFiles :: FileOpS m s => [FilePath] -> Source m QueryResult
openFiles fs =  fs `sourceDirectories` ".txt"
             =$= openFile
             =$= linesOn "\t"
             =$= filterC (\x     -> Prelude.length x == 2)
             =$= mapC    (\[xs,n] -> ( preprocess xs
                                    , xs
                                    , read . unpack $ n :: Integer))

-- * search for pattern parsed by parser `p` and 
-- * sum all of its occurences
queryFiles :: FileOpS m [QueryResult]
           => Parser Text 
           -> Consumer QueryResult m Integer
queryFiles p =  filterC (\(xs,_,_) -> case p <** xs of
                        Left _ -> False
                        _      -> True)
            =$= awaitForever (\t -> do
                    ts <- lift get
                    let ts' = t:ts
                    lift . put $ ts'
                    yield t
                )
            =$= foldlC  (\m (_,_,n) -> m + n) 0




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
    , countp
    , w1
    , w2
    , s1
    , s2
    , p1
    , p2
  ) where


import Control.Monad.State  
import Control.Monad.Trans.Reader

import Data.Conduit 
import Data.Text (Text, unpack, pack)
import Data.Attoparsec.Text hiding (count)


import Core
import Query
import PatternCompiler

{-----------------------------------------------------------------------------
  Score 
------------------------------------------------------------------------------}

w1 :: String -> String -> ReaderT Config IO Output
w1 a1 a2 = do
  p_ws <- pattern weakStrong
  sumCount $ (\p -> p (S a1) (S a2)) <$> p_ws

s1 :: String -> String -> ReaderT Config IO Output
s1 a1 a2 = do
  p_sw <- pattern strongWeak
  sumCount $ (\p -> p (S a1) (S a2)) <$> p_sw

w2 :: String -> String -> ReaderT Config IO Output
w2 = flip w1

s2 :: String -> String -> ReaderT Config IO Output
s2 = flip s1

p1 :: ReaderT Config IO Output
p1 = do
  p_ws <- pattern weakStrong
  sumCount $ (\p -> p Star Star) <$> p_ws

p2 :: ReaderT Config IO Output
p2 = do
  p_sw <- pattern strongWeak
  sumCount $ (\p -> p Star Star) <$> p_sw

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
  (n, ts) <- phrase `query` (ngrams con)
  return (n,ts)

-- * `count` occurences of some word `w` 
-- * in onegram file
count :: String -> ReaderT Config IO Output
count w = do
  let word = compile' w 
  con     <- ask
  (n, ts) <- word `query` [onegram con]
  return (n,ts)




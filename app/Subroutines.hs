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
 
module Subroutines where

import System.Directory
import qualified System.IO as S
import Control.Monad.Trans.Reader

import Lib
import Src


{-----------------------------------------------------------------------------
  Subroutines
------------------------------------------------------------------------------}

-- * create output directory, run `p1` and save results
main_p1 :: Config -> IO ()
main_p1 con = do
  outdir   <- makeDirAtTop "outputs"
  (n, rrs) <- runReaderT p1 con
  let f = outdir ++ "/p-weak-strong.txt"
  writeOutput f (n,rrs)

main_p2 :: Config -> IO ()
main_p2 con = do
  outdir   <- makeDirAtTop "outputs"
  (n, rrs) <- runReaderT p2 con
  let f = outdir ++ "/p-strong-weak.txt"
  writeOutput f (n, rrs)

main_test :: Config -> IO ()
main_test con = do

  outdir   <- makeDirAtTop "outputs"


  let p    = compile "* (,) but not *" (S "good") (S "great")
  (n, rrs) <- runReaderT (countp p) con

  let f    = outdir ++ "/" ++ name p
  writeOutput f (n,rrs)

main_shard :: Int -> Config -> IO ()
main_shard n con = do
  let p:_ = ngrams con
  let o   = p ++ "_shard"
  createDirectoryIfMissing False o
  shardFiles ".txt" n p o
















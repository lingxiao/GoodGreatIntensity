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
 
module MainSubroutines where

import Control.Monad.Trans.Reader

import Core
import Score

{-----------------------------------------------------------------------------
  Subroutines
------------------------------------------------------------------------------}

-- * create output directory, run `p1` and save results
main_p1 :: Config -> IO ()
main_p1 con = do
  outdir   <- makeDirAtTop "output"
  (n, rrs) <- runReaderT p1 con
  let f = outdir ++ "/p-weak-strong.txt"
  saveOutput f (n,rrs)

main_p2 :: Config -> IO ()
main_p2 con = do
  outdir   <- makeDirAtTop "output"
  (n, rrs) <- runReaderT p2 con
  let f = outdir ++ "/p-strong-weak.txt"
  saveOutput f (n, rrs)

main_test :: Config -> IO ()
main_test con = do
    outdir   <- makeDirAtTop "output"
    let tf = outdir ++ "/before_w.txt"
    saveOutput tf (404,[])


    (n, rrs) <- runReaderT (w1 "good" "great") con
    let f = outdir ++ "/p-w1-good-great.txt"
    saveOutput f (n,rrs)

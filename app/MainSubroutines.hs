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

import qualified System.IO as S
import Control.Monad.Trans.Reader

import Lib
import Src


{-----------------------------------------------------------------------------
  Subroutines
------------------------------------------------------------------------------}

out = "outputs"

-- * create output directory, run `p1` and save results
main_p1 :: Config -> IO ()
main_p1 con = do
  outdir   <- makeDirAtTop out
  (n, rrs) <- runReaderT p1 con
  let f = outdir ++ "/p-weak-strong.txt"
  writeOutput f (n,rrs)

main_p2 :: Config -> IO ()
main_p2 con = do
  outdir   <- makeDirAtTop out
  (n, rrs) <- runReaderT p2 con
  let f = outdir ++ "/p-strong-weak.txt"
  writeOutput f (n, rrs)

main_test :: Config -> IO ()
main_test con = do

  writeDummyTo out

  let p    = compile "* (,) but not *" (S "good") (S "great")
  (n, rrs) <- runReaderT (countp p) con

  outdir   <- makeDirAtTop out
  let f    = outdir ++ "/" ++ name p
  writeOutput f (n,rrs)


















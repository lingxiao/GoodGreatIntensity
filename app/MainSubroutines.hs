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
  (n, rrs) <- runReaderT p1 con
  makeDirAtTop (output con)
  let f = output con ++ "/p-weak-strong.txt"
  saveOutput f (n,rrs)

main_p2 :: Config -> IO ()
main_p2 con = do
  (n, rrs) <- runReaderT p2 con
  makeDirAtTop (output con)
  let f = output con ++ "/p-strong-weak.txt"
  saveOutput f (n, rrs)

{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Unzip files
-- | Author  : Xiao Ling
-- | Date    : 8/27/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Untar (
      untar
    , shardAll
    ) where



import Data.Conduit 
import Core
import Conduits

{-----------------------------------------------------------------------------
   top level routines
------------------------------------------------------------------------------}

-- * @Use: run $ untar "/path/to/file" ".gz" ".txt"
-- * Untar all files with extension `e1` found at directory `p`
-- * and save them in the same directory with extension `e2` 
untar :: FileOpS m s => FilePath -> String -> String -> m ()
untar p e1 e2 =  [p] `sourceDirectories` e1
                $$  untarSaveAs e2
                =$= cap


-- * Shard all files with `ext` found at directory `p`
-- * into chunks of 100000 lines each
-- * and save in output directory `o`
shardAll :: FileOpS m s => String -> FilePath -> FilePath -> m ()
shardAll ext p o =  [p] `sourceDirectories` ext
                $$  shardFile ext o 100000
                =$= logm "Sharded all files!"
                =$= cap                



{-----------------------------------------------------------------------------
   untar all files

untarAll :: IO ()
untarAll = do
    run $ untar p1 ".gz" ".txt"
    run $ untar p2 ".gz" ".txt"
    run $ untar p3 ".gz" ".txt"
    run $ untar p4 ".gz" ".txt"
    run $ untar p5 ".gz" ".txt"
------------------------------------------------------------------------------}





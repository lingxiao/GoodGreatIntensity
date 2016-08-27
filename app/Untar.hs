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

module Untar where



import Data.Conduit 
import Core
import Conduits


{-----------------------------------------------------------------------------
   top level routines
------------------------------------------------------------------------------}

-- * @Use: run $ untarAll "/path/to/file" ".gz" ".txt"
-- * Untar all files with extension `e1` found at directory `p`
-- * and save them in the same directory with extension `e2` 
untarAll :: FileOpS m s => FilePath -> String -> String -> m ()
untarAll p e1 e2 =  p `traverseAll` e1
                $$  untarSaveAs e2
                =$= logm "untarred all files!"
                =$= cap



-- * Shard all files with `ext` found at directory `p`
-- * into chunks of 100000 lines each
-- * and save in output directory `o`
shardAll :: FileOpS m s => String -> FilePath -> FilePath -> m ()
shardAll ext p o =  p `traverseAll` ext
                $$  shardFile ext o 100000
                =$= logm "Sharded all files!"
                =$= cap                






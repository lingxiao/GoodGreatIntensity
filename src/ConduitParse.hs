{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Parsing a text file stream using conduit combinators
-- | Author  : Xiao Ling
-- | Date    : 8/16/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module ConduitParse where


import Prelude hiding           (readFile, writeFile ,
                                 lines   ,           )

import Data.Text hiding         (lines)
import Data.ByteString          (ByteString, readFile)
import Data.Conduit.Binary      (sourceFile, sinkFile)
import Data.Conduit.Text
import Conduit hiding           (sourceDirectory     ,
                                 sourceFile          )


import Core
import ConduitLib

{-----------------------------------------------------------------------------
   redo using conduit

   openByteString >> convertToText >> lines >> mapMaybe splitAndParse
------------------------------------------------------------------------------}


ps, ps1 :: FilePath
ps  = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/short/"
ps1 = ps ++ "1gm.txt"

-- * just consider opening a file in bytestring,
-- * and then 


--foo :: FileOpS m s => m ()
foo :: FileOpS m s => m ()
foo =  runConduit 
    $  sourceFile ps1
   =$= linesOn "\t"
   =$= awaitForever (\[w,n] -> do
            liftIO . print $ w)



--bar =  ps `traverseAll` ".txt"
    --$$ awaitForever (\p -> )









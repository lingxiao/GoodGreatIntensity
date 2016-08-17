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
                                 lines   , filter    )

import Data.Text hiding         (lines   , filter    )
import Data.ByteString          (ByteString, readFile)
import Data.Conduit.List
import Data.Conduit.Binary      (sourceFile, sinkFile)
import Data.Conduit.Text
import Conduit hiding           (sourceDirectory     ,
                                 sourceFile          )


import Core
import ConduitLib

{-----------------------------------------------------------------------------
   redo using conduit

   openByteString >> convertToText >> lines >> mapMaybe splitAndParse

   NOTE: This whole thing will be factored away
------------------------------------------------------------------------------}


ps, ps1 :: FilePath
ps  = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/short/"
ps1 = ps ++ "1gm.txt"

p1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/1gms/"
p2 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/2gms/"
p  = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/"


-- * just consider opening a file in bytestring,
-- * and then 

-- * where you left off: so you have a version working
-- *                     now you need to actually parse the first item 
-- *                     also consider how you would traverse all documents
-- *                     for all occurences of the pattern
bar :: FileOpS m s => m ()
bar =   runConduit 
    $   p `traverseAll` ".txt"
    =$= openFile
    =$= linesOn "\t"
    =$= filter       (\x     -> Prelude.length x == 2  )
    =$= mapC         (\[w,n] -> [unpack w, unpack n]   )
    =$= filter       (\[w,n] -> w == "Caribbean Prints")  -- * replace with parser
    =$= awaitForever (\[w, n]-> do
           liftIO banner
           liftIO . print $ w)


foo :: FileOpS m s => m ()
foo =  runConduit 
    $  sourceFile ps1
   =$= linesOn "\t"
   =$= filter (\[w,n] -> w == pack "swimsjuit")
   =$= awaitForever (\[w,n] -> do
             liftIO . print $ w
       )










{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Collect statistics
-- | Author  : Xiao Ling
-- | Date    : 8/17/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Count where

import Prelude hiding (filter)

import Control.Monad.IO.Class 

import Data.Conduit 
import Data.Conduit.Text
import Data.Conduit.List


import Core
import Parsers
import Conduits


p  = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/5gmsG/"
p5 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/5gms/"

bar :: FileOpS m s => m ()
bar =   runConduit 
    $   p `traverseAll` ".txt"
    =$= openFile
    =$= linesOn "\t"
    =$= filter       (\x     -> Prelude.length x == 2)
    =$= logi
    =$= filter       (\[w,n] -> case "good" `butNot` "great" <** w of
                                    Just _ -> True
                                    _      -> False
                     )
    =$= awaitForever (\[w,n] -> do
           liftIO banner
           liftIO . print $ "filtered value: "
           liftIO . print $ [w,n])
    =$= cap


{-----------------------------------------------------------------------------

import Prelude hiding           (readFile, writeFile ,
                                 lines   ,           )

import System.FilePath
import Control.Monad.IO.Class   (MonadIO, liftIO     )
import Control.Monad

import Data.Conduit 
import Conduit hiding           (sourceDirectory     ,
                                 sourceFile          )

import Data.Conduit.Filesystem  (sourceDirectory     )
import Data.Conduit.Binary      (sourceFile, sinkFile)
import Data.Text.Lazy.IO        

-- import Data.List.Split
import qualified Data.Text.Lazy as L

-- * new stuff, need to move into ngram parse
import Data.Text hiding          (lines)
import Data.Conduit.Text
import Data.Conduit.List



   redo using conduit

   openByteString >> convertToText >> lines >> mapMaybe splitAndParse

   NOTE: This whole thing will be factored away


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
------------------------------------------------------------------------------}



{-----------------------------------------------------------------------------


-- * This whole thing is about to be refactored
   Counting 

-- * Problem: case and stuff
-- * Count the number of occurences of string `w`
cntW :: FileOpS m s 
     => String 
     -> Conduit FilePath m Int

cntW w = awaitForever $ \p -> do

    let name = dropExtension . takeFileName $ p

    ts <- liftIO . readFile $ p

    case (cnt w) <** ts of
        Nothing     -> logger w name 0 >> yield 0
        Just (_, n) -> logger w name n >> yield n

        where logger w name n = do
              liftIO $ banner 
              liftIO . print $ w
                    ++ " occurs " ++ show n
                    ++ " times in document " ++ name


-- openByteString >> convertToText >> lines >> mapMaybe splitAndParse


-- * use this to sanity check that only one
-- * of the docs should have what you're looking for
foo :: FileOpS m s => FilePath -> String -> m ()
foo p w = p `traverseAll` ".txt"
      $$  cntW w
      =$= cap 


mainShard :: IO ()
mainShard = do
    run $ shardAll ".txt" p3 p3o
    run $ shardAll ".txt" p4 p4o
    run $ shardAll ".txt" p5 p5o

p1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/1gms/"
p2 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/2gms/"
p3 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/3gms/"
p4 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/4gms/"
p5 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/5gms/"

p2o = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/2gmsnew/"
p3o = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/3gmsnew/"
p4o = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/4gmsnew/"
p5o = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/5gmsnew/"


------------------------------------------------------------------------------}



{-
pip1 :: FileOpS m s => m Int
pip1 =  runConduit $ path2 `traverseAll` ".txt"
    =$= mapMC (\p -> liftIO $ fmap (length . B.lines) $ B.readFile p)
    =$= foldlC (+) 0
-}













{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Collect statistics
-- | Author  : Xiao Ling
-- | Date    : 8/15/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Count where


import Prelude hiding           (readFile, writeFile )

import System.FilePath
import Control.Monad.IO.Class   (MonadIO, liftIO     )

import Data.Conduit 
import Conduit hiding           (sourceDirectory     ,
                                 sourceFile          )

import Data.Conduit.Filesystem  (sourceDirectory     )
import Data.Conduit.Binary      (sourceFile, sinkFile)
import Data.Text.Lazy.IO        


import Data.List.Split
import qualified Data.Text.Lazy as L


import Core
import ConduitLib
import NgramParser


p1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/1gms/"
p2 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/2gms/"
p3 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/3gms/"
p4 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/4gms/"
p5 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/5gms/"

{-----------------------------------------------------------------------------
   Counting 
------------------------------------------------------------------------------}

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


-- * use this to sanity check that only one
-- * of the docs should have what you're looking for
foo :: FileOpS m s => FilePath -> String -> m ()
foo p w = p `traverseAll` ".txt"
    $$  cntW w
    =$= cap 






{-
pip1 :: FileOpS m s => m Int
pip1 =  runConduit $ path2 `traverseAll` ".txt"
    =$= mapMC (\p -> liftIO $ fmap (length . B.lines) $ B.readFile p)
    =$= foldlC (+) 0
-}













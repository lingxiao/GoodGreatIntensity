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


import Prelude hiding           (readFile            )

import System.FilePath
import Control.Monad.IO.Class   (MonadIO, liftIO     )

import Data.Conduit 
import Conduit hiding           (sourceDirectory     ,
                                 sourceFile          )

import Data.Conduit.Filesystem  (sourceDirectory     )
import Data.Conduit.Binary      (sourceFile, sinkFile)
import Data.Text.Lazy.IO        


import Core
import ConduitLib
import ParserLib


p1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/1gms/"
p2 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/2gms/"
p3 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/3gms/"
p4 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/4gms/"
p5 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/5gms/"

{-----------------------------------------------------------------------------
   Counting 
------------------------------------------------------------------------------}

-- * Count the number of occurences of string `xs`
cntW :: FileOpS m s 
     => String 
     -> Conduit FilePath m Int

cntW xs = awaitForever $ \p -> do

    let name = dropExtension . takeFileName $ p

    f <- liftIO . readFile $ p

    case search f xs of
        Nothing     -> logger xs name 0 >> yield 0
        Just (_, n) -> logger xs name n >> yield n

        where logger xs name n = do
              liftIO $ banner 
              liftIO . print $ xs
                    ++ " occurs " ++ show n
                    ++ " times in document " ++ name


-- * use this to sanity check that only one
-- * of the docs should have what you're looking for
foo :: FileOpS m s => FilePath -> String -> m ()
foo p xs = p `traverseAll` ".txt"
    $$  cntW xs
    =$= cap 









{-
pip1 :: FileOpS m s => m Int
pip1 =  runConduit $ path2 `traverseAll` ".txt"
    =$= mapMC (\p -> liftIO $ fmap (length . B.lines) $ B.readFile p)
    =$= foldlC (+) 0
-}













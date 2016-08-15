{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Parsers to crawl google ngram corpus
-- | Author  : Xiao Ling
-- | Date    : 8/14/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module NgramParser.hs where


import Control.Monad
import Control.Applicative

import Data.Attoparsec.Text.Lazy
import Data.Attoparsec.Combinator
import Data.Text hiding (foldr)
import qualified Data.Text.Lazy as L (Text, pack)

import Core

{-----------------------------------------------------------------------------
   Patterns used in scoring expression
------------------------------------------------------------------------------}

-- * search for first occurence of `w` and output its count
cnt :: String -> Pattern
cnt w = do
    pUntil w 
    string . pack $ w
    string "\t"
    n <- natural 
    string "\n"
    return (w, read n)





{-----------------------------------------------------------------------------
   Parser utils 
------------------------------------------------------------------------------}

-- * Parse a natural number
natural :: Parser String
natural = many1' digit

-- * skip over all words in Text stream until the word we want
pUntil :: String -> Parser String 
pUntil = manyTill anyChar . lookAhead . string . pack 

notSpace :: Parser Char
notSpace = notChar ' '


{-----------------------------------------------------------------------------
   adhoc tests
------------------------------------------------------------------------------}

-- * TODO: also need a parser that 
-- * will match the Psw and Psw patterns!!

-- * should succed
t1 = L.pack "hello\t999\nworld\t\900"
t2 = L.pack "world\t\900\nhello\t999\n"

-- * should fail
t3 = L.pack "world\t\900\nhello world\t999\n"
t4 = L.pack "hello world\t999\nworld\t\900"
t5 = L.pack "world hello\t999\nworld\t\900"

w1 :: Pattern
w1 = cnt "hello"    







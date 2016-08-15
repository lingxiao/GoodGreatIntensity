{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : A library of attoparsec parsers
-- | Author  : Xiao Ling
-- | Date    : 8/14/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module ParserLib where

import Prelude hiding (takeWhile)

import Control.Applicative

import Data.Char
import Data.Text hiding (takeWhile, foldr)
import qualified Data.Text.Lazy as L (Text, pack)
import Data.Attoparsec.Text.Lazy
import Data.Attoparsec.Combinator

import Core

{-----------------------------------------------------------------------------
   Attoparsec routines specific for this application
------------------------------------------------------------------------------}

-- * search file `f` for word `w` and output its frequency
search :: L.Text -> String -> Maybe (String, Int)
search f w = case parse (occur w) f of
    Done _ r   -> Just r
    _          -> Nothing

---- * discard all text until word `w` occurs, and find its only field `n`
occur :: String -> Parser (String, Int)
occur w = do
    pUntil w
    string . pack $ w
    string "\t"
    n <- natural 
    string "\n"
    return (w, read n)


{-----------------------------------------------------------------------------
   Attoparsec combinators 
------------------------------------------------------------------------------}

-- * Parse a natural number
natural :: Parser String
natural = many1' digit

-- * skip over all words in Text stream until the word we want
pUntil :: String -> Parser String 
pUntil = manyTill anyChar . lookAhead . string . pack




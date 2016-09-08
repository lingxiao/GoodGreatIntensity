{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Compile string patterns into Attoparsec parsers
-- | Author  : Xiao Ling
-- | Date    : 9/7/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module PatternCompiler (

    compile
  , token
  , tokenizer

  , (<**)
  , name

  ) where

import Data.List.Split 
import Data.Attoparsec.Text
import Data.Text hiding (foldr, splitOn, init, concat, last)

import Core
import Control.Applicative
import Parsers


{-----------------------------------------------------------------------------
    Top level function
------------------------------------------------------------------------------}

-- * given an expression, output parser
compile :: String -> Pattern
compile = compiler . tokenizer


{-----------------------------------------------------------------------------
    Tokenizer
------------------------------------------------------------------------------}

-- * `token`ize a string
-- * note if `token` sees a string `xs` it does not recognize,
-- * it just outputs a `Word xs`
-- * TODO: quick and dirty here, consider doing something real
token :: String -> Token
token "*"    = Hole
token ","    = Comma
token "(,)"  = OptComma
token xs     = case splitOn "/" xs of
  y:ys  -> Word (stripParens y) `catOr` ys
  _     -> Word $ stripParens xs

catOr :: Token -> [String] -> Token
catOr t = foldr (\x ts -> ts `Or` Word (stripParens x)) t

-- * maps a string to some set of tokens
tokenizer :: String -> [Token]
tokenizer = fmap token . concat . fmap recoverComma . splitOn " "


{-----------------------------------------------------------------------------
    Compiler
------------------------------------------------------------------------------}

-- * compile a list of tokens into binary pattern `BinPattern`
-- * note by construction this function fails with identity parser under (<+>)
compiler :: [Token] -> (String -> String -> Parser Text)
compiler ts = \u v -> go [u,v] ts

-- * Given stack of strings `w:ws` as input to binary pattern,
-- * and list of tokens `t:ts`, create a parser
-- * if the stack is empty before tokens are, then all
-- * tokens `Hole` is sent to parser `star`
-- * Note the privilged string `w` "_*_" denotes wildcard pattern,
-- * so "_*_" maps token `Hole` to the parser `star`
go :: [String] -> [Token] -> Parser Text
go (w:ws) (t:ts) | t == Hole = if w == wildCardSym then 
                               toP  t <+> go ws ts else
                               word w <+> go ws ts
                 | otherwise = toP  t <+> go (w:ws) ts
go []     (t:ts)             = toP  t <+> go []     ts
go _      []                 = pzero                


-- * convert token to parser, note `Hole` is sent to `star` which accept
-- * any string of alphabetical symbols
toP :: Token -> Parser Text
toP (Word xs)  = word xs
toP Hole       = star
toP Slash      = pzero
toP OptComma   = comma
toP Comma      = word ","
toP (Or t1 t2) = toP t1 <|> toP t2


{-----------------------------------------------------------------------------
    Utils
------------------------------------------------------------------------------}

recoverComma :: String -> [String]
recoverComma []                  = []
recoverComma xs | last xs == ',' = [init xs, ","]
                | otherwise      = [xs]

-- * aggressively remove all occurences of "(" and/or ")" in a string
stripParens :: String -> String
stripParens = foldr strip mempty
    where strip c cs | c == '('   = cs
                     | c == ')'   = cs
                     | otherwise  = c:cs






















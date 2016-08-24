{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : a library of parser utility functions and parser combinators
-- | Author  : Xiao Ling
-- | Date    : 8/14/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Parsers where

import Prelude hiding   (concat, takeWhile)
import Control.Monad
import Control.Applicative

import Data.Char
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Text hiding (foldr)

{-----------------------------------------------------------------------------
   Run parser, and a parser combinator
------------------------------------------------------------------------------}

-- * Preprocess text `t` and parse with `p`
infixr 8 <**
(<**) :: Parser a -> Text -> Maybe a
p <** t = case parse p t of
    Done _ r   -> Just r
    Fail _ _ _ -> Nothing
    Partial c -> case c "" of
        Done _ r -> Just r
        _        -> Nothing

infixr 9 <+>
(<+>) :: Parser Text -> Parser Text -> Parser Text
p <+> q = (\u v -> concat [u, pack " ", v]) <$> p <*> q

{-----------------------------------------------------------------------------
   Basic parsers
------------------------------------------------------------------------------}

-- * TODO: word "hello" will match "hellooooo"
-- * parse some string `w` with 0 or more spaces infront

-- * TODO: need to consume as many `term` as possible
-- * and make sure what comes immedateily after the term is
-- * not an alphabetical item


word :: String -> Parser Text
word w = spaces *> string (pack w) <* lookAhead term 
  where term = (const ()) <$> notAlphaNum <|> endOfInput

-- * parse any alphabetical string with 0 or more spaces infront
anyWord :: Parser Text
anyWord = spaces *> takeWhile1 isAlpha

-- * parse one or more spaces and ouput one space
spaces1 :: Parser Text
spaces1 = tok " " <$> many1' space

-- * parse zero or more spaces and ouput one space
spaces :: Parser Text
spaces = tok " " <$> many' space

{-----------------------------------------------------------------------------
  Utility
------------------------------------------------------------------------------}

tok :: String -> a -> Text
tok t = const . pack $ t

notAlphaNum :: Parser Char
notAlphaNum = satisfy (not . isAlphaNum)





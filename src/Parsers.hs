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
import Data.Text hiding (foldr, takeWhile)

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

-- * parse one or more spaces and ouput one space
spaces1 :: Parser Text
spaces1 = tok " " <$> many1' space

-- * parse zero or more spaces and ouput one space
spaces :: Parser Text
spaces = tok " " <$> many' space


-- * look ahead all nonAlpha symbols and end in space or eoi
eow :: Parser Text
eow =   lookAhead 
    $   pack 
    <$> many' notAlphaDigitSpace <* (const () <$> space <|> endOfInput)

word :: String -> Parser Text
word w = spaces *> string (pack w) <* eow


-- * parse any alphabetical string with 0 or more spaces infront
anyWord :: Parser Text
anyWord = spaces *> takeWhile1 isAlpha


{-----------------------------------------------------------------------------
  Utility
------------------------------------------------------------------------------}

tok :: String -> a -> Text
tok t = const . pack $ t

notAlphaNum :: Parser Char
notAlphaNum = satisfy (not . isAlphaNum)

-- * things not allowed: alphabets, numbers, space
notAlphaDigitSpace :: Parser Char
notAlphaDigitSpace = satisfy (\c -> not (isDigit c || isAlpha c || isSpace c))




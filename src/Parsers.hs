{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : a library of ngram parsers for google ngram corpus
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
   Run Parser and Parser Combinator
------------------------------------------------------------------------------}

infixr 8 <**
(<**) :: Parser a -> Text -> Maybe a
p <** t = case parse p t of
    Done _ r -> Just r
    _        -> Nothing

infixr 9 <+>
(<+>) :: Parser Text -> Parser Text -> Parser Text
p <+> q = (\u v -> concat [u, pack " ", v]) <$> p <*> q


{-----------------------------------------------------------------------------
   Weak-Strong pattens
------------------------------------------------------------------------------}

--    w (,) but not s
butNot :: String -> String -> Parser Text
butNot w s = word w <+> comma' <+> but_ <+> not_ <+> word s

--     * (,) but not *
butNot' :: Parser Text  
butNot' = star <+> comma' <+> but_ <+> not_ <+> star


althoughNot :: String -> String -> Parser Text
althoughNot w s = word w <+> comma' <+> although_ <+> not_ <+> word s

althoughNot' :: Parser Text
althoughNot' = star <+> comma' <+> although_ <+> not_ <+> star


{-----------------------------------------------------------------------------
   words
------------------------------------------------------------------------------}

comma, but_, not_, if_, or_
     , although_, only_, just_, still_  :: Parser Text
comma     = word ","
but_      = word "but"
not_      = word "not"
if_       = word "if"
or_       = word "or"
although_ = word "although"
only_     = word "only"
just_     = word "just"
still_    = word "still"

-- * parses any word and outputs "*"
star :: Parser Text
star = tok "*" <$> anyWord

-- * next char could either be a comma or 
-- * one or more spaces
comma' :: Parser Text
comma' = tok "(,)" <$> (comma <|> spaces1)

{-----------------------------------------------------------------------------
   Basic parsers
------------------------------------------------------------------------------}

-- * parse some string `w` with 0 or more spaces infront
word :: String -> Parser Text
word w = spaces *> string (pack w)

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





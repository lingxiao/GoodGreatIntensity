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
   Run Parser
------------------------------------------------------------------------------}

infixr 8 <**
(<**) :: Parser a -> Text -> Maybe a
p <** t = case parse p t of
    Done _ r -> Just r
    _        -> Nothing

{-----------------------------------------------------------------------------
   Application specific parsers
------------------------------------------------------------------------------}

--    w (,) but not s
butNot :: String -> String -> Parser Text
butNot w s = out (w ++ " (,) but not " ++ s)
           $ word w *> commaS *> but_ *> not_ *> word s

--     * (,) but not *
butNot' :: Parser Text  
butNot' = out "* (,) but not *"
        $ star *> commaS *> but_ *> not_ *> star

althoughNot :: String -> String -> Parser Text
althoughNot w s = out (w ++ " (,) although not " ++ s)
                $ word w *> commaS *> although_ *> not_ *> word s

althoughNot' :: Parser Text
althoughNot' = out ("* (,) although not *")
             $ star *> commaS *> although_ *> not_ *> star


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
commaS :: Parser Text
commaS = tok "(,)" <$> (comma <|> spaces1)

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

out :: String -> Parser a -> Parser Text
out xs = fmap (\_ -> pack xs)





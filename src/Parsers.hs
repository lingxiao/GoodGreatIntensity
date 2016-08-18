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
   Weak-Strong patterns
------------------------------------------------------------------------------}

--    w (,) but not s
butNot :: String -> String -> Parser Text
butNot w s      = word w <+> butNot_ <+> word s

--     * (,) but not *
butNot' :: Parser Text  
butNot'         = star <+> butNot_ <+> star


althoughNot :: String -> String -> Parser Text
althoughNot w s = word w <+> althoughNot_ <+> word s

althoughNot' :: Parser Text
althoughNot'    = star <+> althoughNot_ <+> star


{-----------------------------------------------------------------------------
   Relationships
------------------------------------------------------------------------------}

butNot_ , althoughNot_, thoughNot_ , andorEven_
        , andorAlmost_ :: Parser Text 
butNot_       = comma' <+> but_           <+> not_
althoughNot_  = comma' <+> although_      <+> not_
thoughNot_    = comma' <+> though_        <+> not_
andorEven_    = comma' <+> (and_ <|> or_) <+> even_
andorAlmost_  = comma' <+> (and_ <|> or_) <+> almost_

{-----------------------------------------------------------------------------
   words
------------------------------------------------------------------------------}

comma, but_, not_, and_, if_, or_, almost_, even_
     , although_, though_, only_, just_, still_  :: Parser Text
comma     = word ","
but_      = word "but"
and_      = word "and"
not_      = word "not"
if_       = word "if"
or_       = word "or"
although_ = word "although"
though_   = word "though"
only_     = word "only"
even_     = word "even"
just_     = word "just"
still_    = word "still"
almost_   = word "almost"



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

-- * word "hello" <* lookahead (noneOf ['A'..'z'])




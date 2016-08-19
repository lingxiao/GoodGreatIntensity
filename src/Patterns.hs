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

module Patterns where

import Prelude hiding   (concat, takeWhile)
import Control.Monad
import Control.Applicative

import Data.Char
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Text hiding (foldr)

import Parsers

{-----------------------------------------------------------------------------
   Weak-strong patterns
------------------------------------------------------------------------------}

--    w (,) but not s
butNot :: String -> String -> Parser Text
butNot w s = word w <+> butNot_ <+> word s

--     * (,) but not *
butNot' :: Parser Text  
butNot' = star <+> butNot_ <+> star


althoughNot :: String -> String -> Parser Text
althoughNot w s = word w <+> althoughNot_ <+> word s

althoughNot' :: Parser Text
althoughNot'    = star <+> althoughNot_ <+> star

thoughNot :: String -> String -> Parser Text
thoughNot w s = word w <+> thoughNot_ <+> word s

thoughNot' :: Parser Text
thoughNot'    = star   <+> thoughNot_ <+> star

andorEven :: String -> String -> Parser Text
andorEven w s = word w <+> andorEven_ <+> word s

andorEven' :: Parser Text
andorEven'    = star   <+> andorEven_ <+> star

andorAlmost :: String -> String -> Parser Text
andorAlmost w s = word w <+> andorAlmost_ <+> word s

andorAlmost' :: Parser Text
andorAlmost'    = star   <+> andorAlmost_ <+> star

notOnly :: String -> String -> Parser Text
notOnly w s    = not_ <+> only_ <+> word w <+> but_ <+> word s

notOnly' :: Parser Text
notOnly'       = not_ <+> only_ <+> star   <+> but_ <+> star

notJust :: String -> String -> Parser Text
notJust w s    = not_ <+> just_ <+> word w <+> but_ <+> word s

notJust' :: Parser Text
notJust'       = not_ <+> just_ <+> star   <+> but_ <+> star


{-----------------------------------------------------------------------------
   Strong-weak patterns
------------------------------------------------------------------------------}




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
   Sords
------------------------------------------------------------------------------}

but_, not_, and_, if_, or_, almost_, even_
    , although_, though_, only_, just_, still_  :: Parser Text
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

{-----------------------------------------------------------------------------
   Punctuations
------------------------------------------------------------------------------}

-- * next char could either be a comma or 
-- * one or more spacesW
comma' :: Parser Text
comma' = tok "(,)" <$> (comma <|> spaces1)

comma :: Parser Text
comma  = word ","



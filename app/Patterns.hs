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
   Types
------------------------------------------------------------------------------}

type Name      = String
type Adjective = String
type Pws       = Parser Text -> Parser Text -> (Name, [Parser Text])
type Psw       = Parser Text -> Parser Text -> (Name, [Parser Text])


{-----------------------------------------------------------------------------
   Pattern Lists
------------------------------------------------------------------------------}

p_weakStrong :: Pws
p_weakStrong = \u v -> 
    ( "p_weakStrong_" ++ name u ++ "_" ++ name v
    , (\p -> p u v ) <$>
    [ butNot
    , althoughNot
    , thoughNot
    , andorEven
    , andorAlmost
    , notOnly
    , notJust]
    )

p_strongWeak :: Psw
p_strongWeak = \u v -> 
    ( "p_strongWeak_" ++ name u ++ "_" ++ name v
    , (\p -> p u v ) <$>
    [ notJust1
    , notButJust
    , notStill
    , notButStill
    , notAlthoughStill
    , notThoughStill
    , orVery]
    )

{-----------------------------------------------------------------------------
   Weak-strong patterns
------------------------------------------------------------------------------}

--    w (,) but not s
butNot :: Parser Text -> Parser Text -> Parser Text
butNot w s      = w <+> comma' <+> but_ <+> not_ <+> s

althoughNot :: Parser Text -> Parser Text -> Parser Text
althoughNot w s = w <+> comma' <+> although_ <+> not_ <+> s

thoughNot :: Parser Text -> Parser Text -> Parser Text
thoughNot w s   = w <+> comma' <+> though_  <+> not_ <+> s

andorEven :: Parser Text -> Parser Text -> Parser Text
andorEven w s   = w <+> comma' <+> (and_ <|> or_) <+> even_ <+> s

andorAlmost :: Parser Text -> Parser Text -> Parser Text
andorAlmost w s = w <+> comma' <+> (and_ <|> or_) <+> almost_ <+> s

notOnly :: Parser Text -> Parser Text -> Parser Text
notOnly w s     = not_ <+> only_ <+> w <+> but_ <+> s

notJust :: Parser Text -> Parser Text -> Parser Text
notJust w s     = not_ <+> just_ <+> w <+> but_ <+> s


{-----------------------------------------------------------------------------
   Strong-weak patterns
------------------------------------------------------------------------------}

notJust1 :: Parser Text -> Parser Text -> Parser Text
notJust1 s w = not_ <+> s <+> comma' <+> just_ <+> w

notButJust :: Parser Text -> Parser Text -> Parser Text
notButJust s w = not_ <+> s <+> comma' <+> but_ <+> just_ <+> w

notStill :: Parser Text -> Parser Text -> Parser Text
notStill s w = not_ <+> s <+> comma' <+> still_ <+> w

notButStill :: Parser Text -> Parser Text -> Parser Text
notButStill s w = not_ <+> s <+> comma' <+> but_ <+> still_ <+> w

notAlthoughStill :: Parser Text -> Parser Text -> Parser Text
notAlthoughStill s w = not_ <+> s <+> comma' <+> although_ <+> still_ <+> w

notThoughStill :: Parser Text -> Parser Text -> Parser Text
notThoughStill s w = not_ <+> s <+> comma' <+> though_ <+> still_ <+> w

orVery :: Parser Text -> Parser Text -> Parser Text
orVery s w = s <+> comma' <+> or_ <+>  very_ <+> w

{-----------------------------------------------------------------------------
   Words
------------------------------------------------------------------------------}

but_, not_, and_, if_, or_, almost_, even_
    , although_, though_, only_, just_, still_, very_ :: Parser Text
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
very_     = word "very"


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




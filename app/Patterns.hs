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

type Adjective = String
type Name      = String

type Pattern   = (Name, Parser Text)

type Pws       = Adjective -> Adjective -> (Name, [Pattern])
type Psw       = Adjective -> Adjective -> (Name, [Pattern])


{-----------------------------------------------------------------------------
   Pattern Lists
   TODO: factor all naming patterns into some new monad type 
         wrapping parser
------------------------------------------------------------------------------}

word' :: String -> Parser Text
word' xs = if xs == "*" then star else word xs


p_weakStrong :: Pws
p_weakStrong = \u v -> 
    ( "p_weakStrong_" ++ u ++ "_" ++ v
    , [ (u ++ "_but_not_"      ++ v        , word' u `butNot`      word' v)
      , (u ++ "_although_not_" ++ v        , word' u `althoughNot` word' v)
      , (u ++ "_though_not_"   ++ v        , word' u `thoughNot`   word' v)
      , (u ++ "_andor_even_"   ++ v        , word' u `andorEven`   word' v)
      , (u ++ "_andor_almost_" ++ v        , word' u `andorAlmost` word' v)
      , ("not_only_" ++ u ++ "_but_" ++ v  , word' u `notOnly`     word' v)
      , ("not_just_" ++ u ++ "_but_" ++ v  , word' u `notJust`     word' v)
      ]
    )    


p_strongWeak :: Psw
p_strongWeak = \u v -> 
    ("p_strongWeak_" ++ u ++ "_" ++ v
    , [ ("not_" ++ u ++ "_just_"           ++ v    , word' u `notJust1`        word' v)
      , ("not_" ++ u ++ "but_just_"        ++ v    , word' u `notButJust`      word' v)
      , ("not_" ++ u ++ "_still_"          ++ v    , word' u `notStill`        word' v)      
      , ("not_" ++ u ++ "_but_still_"      ++ v    , word' u `notButStill`     word' v)
      , ("not_" ++ u ++ "_although_still_" ++ v    , word' u `notAlthoughStill`word' v)
      , ("not_" ++ u ++ "_though_still_"   ++ v    , word' u `notThoughStill`  word' v)
      , (u ++ "_or_very_" ++ v                     , word' u `orVery`          word' v)
      ]
    )


{-----------------------------------------------------------------------------
   Weak-strong patterns
------------------------------------------------------------------------------}

--    w (,) but not s
butNot :: Parser Text -> Parser Text -> Parser Text
butNot w s      = w <+> butNot_ <+> s

althoughNot :: Parser Text -> Parser Text -> Parser Text
althoughNot w s = w <+> althoughNot_ <+> s

thoughNot :: Parser Text -> Parser Text -> Parser Text
thoughNot w s   = w <+> thoughNot_ <+> s

andorEven :: Parser Text -> Parser Text -> Parser Text
andorEven w s   = w <+> andorEven_ <+> s

andorAlmost :: Parser Text -> Parser Text -> Parser Text
andorAlmost w s = w <+> andorAlmost_ <+> s

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




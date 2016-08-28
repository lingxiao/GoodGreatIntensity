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

module Patterns (

      Adjective
    , Name
    , Pattern
    , WildPattern
    , Pws
    , Psw
    , PwsStar
    , PswStar

    , p_weakStrong
    , p_strongWeak
    , p_weakStrongStar
    , p_strongWeakStar

    ) where

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

type Adjective    = String

type Name         = String -> String -> String

type Pattern      = (Name, Adjective -> Adjective -> Parser Text)
type WildPattern  = (Name, Parser Text                    )

type Pws          = (Name, [Pattern])
type Psw          = (Name, [Pattern])

type PwsStar      = (Name, [WildPattern])
type PswStar      = (Name, [WildPattern])


{-----------------------------------------------------------------------------
   Pattern Lists
------------------------------------------------------------------------------}


p_weakStrong :: Pws
p_weakStrong     = (\u v -> "p_weakStrong_" ++ u ++ "_" ++ v
                   , [ (\u v -> u ++ "_but_not_"     ++ v
                   , butNot                                  )
                   , (\u v -> u ++ "_although_not_"++ v
                   , althoughNot                             )
                   , (\u v -> u ++ "_though_not_"  ++ v
                   , thoughNot                               )
                   , (\u v -> u ++ "_andor_even_"  ++ v
                   , andorEven                               )
                   , (\u v -> u ++ "_andor_almost_"++ v  
                   , andorAlmost                             )
                   , (\u v -> "not_only_" ++ u ++ "_but_" ++ v
                   , notOnly                                 )
                   , (\u v -> "not_just_" ++ u ++ "_but_" ++ v
                   , notJust                                 )
                   ])

p_weakStrongStar :: PwsStar
p_weakStrongStar = (\u v -> "p_weakStrongStar_" ++ u ++ "_" ++ v
                   , [ (\u v -> "star_but_not_star"
                   , butNot'                                 )
                   , (\u v -> "star_although_not_star"
                   , althoughNot'                            )
                   , (\u v -> "star_though_not_star"
                   , thoughNot'                              )
                   , (\u v -> "star_andor_even_star"
                   , andorEven'                              )
                   , (\u v -> "star_andor_almost_star"
                   , andorAlmost'                            )
                   , (\u v -> "not_only_star_but_star"
                   , notOnly'                                )
                   , (\u v -> "not_just_star_but_star"
                   , notJust'                                )
                   ])



p_strongWeak :: Psw
p_strongWeak    =  (\u v -> "p_strongWeak_" ++ u ++ "_" ++ v
                   , [ (\u v -> "not_" ++ u ++ "_just_" ++ v
                   , notJust1                                      )
                   , (\u v -> "not_" ++ u ++ "but_just_" ++ v
                   , notButJust                                    )
                   , (\u v -> "not_" ++ u ++ "_still_" ++ v
                   , notStill                                      )
                   , (\u v -> "not_" ++ u ++ "_but_still_" ++ v
                   , notButStill                                   )
                   , (\u v -> "not_" ++ u ++ "_although_still_" ++ v
                   , notAlthoughStill                              )
                   , (\u v -> "not_" ++ u ++ "_though_still_" ++ v
                   , notThoughStill                                )
                   , (\u v -> u ++ "_or_very_" ++ v
                   , orVery                                        )
                   ])


p_strongWeakStar :: PswStar
p_strongWeakStar = (\u v -> "p_strongWeakStar_" ++ u ++ "_" ++ v
                   , [ (\u v -> "not_star_just_star"
                   , notJust1'                                     )
                   , (\u v -> "not_star_but_just_star"
                   , notButJust'                                   )
                   , (\u v -> "not_star_still_star"
                   , notStill'                                     )
                   , (\u v -> "not_star_but_still_star"
                   , notButStill'                                  )
                   , (\u v -> "not_star_although_still_star"
                   , notAlthoughStill'                             )
                   , (\u v -> "not_star_though_still_star"
                   , notThoughStill'                               )
                   , (\u v -> "star_or_very_star"
                   , orVery'                                       )
                   ])
{-----------------------------------------------------------------------------
   Weak-strong patterns
------------------------------------------------------------------------------}


--    w (,) but not s
butNot :: Adjective -> Adjective -> Parser Text
butNot w s      = word w <+> butNot_ <+> word s

althoughNot :: Adjective -> Adjective -> Parser Text
althoughNot w s = word w <+> althoughNot_ <+> word s

thoughNot :: Adjective -> Adjective -> Parser Text
thoughNot w s   = word w <+> thoughNot_ <+> word s

andorEven :: Adjective -> Adjective -> Parser Text
andorEven w s   = word w <+> andorEven_ <+> word s

andorAlmost :: Adjective -> Adjective -> Parser Text
andorAlmost w s = word w <+> andorAlmost_ <+> word s

notOnly :: Adjective -> Adjective -> Parser Text
notOnly w s     = not_ <+> only_ <+> word w <+> but_ <+> word s

notJust :: Adjective -> Adjective -> Parser Text
notJust w s     = not_ <+> just_ <+> word w <+> but_ <+> word s

--  * wildcard patterns
butNot' :: Parser Text  
butNot'      = star <+> butNot_ <+> star

althoughNot' :: Parser Text
althoughNot' = star <+> althoughNot_ <+> star

thoughNot' :: Parser Text
thoughNot'   = star   <+> thoughNot_ <+> star

andorEven' :: Parser Text
andorEven'   = star   <+> andorEven_ <+> star

andorAlmost' :: Parser Text
andorAlmost'  = star   <+> andorAlmost_ <+> star

notOnly' :: Parser Text
notOnly'      = not_ <+> only_ <+> star   <+> but_ <+> star

notJust' :: Parser Text
notJust'      = not_ <+> just_ <+> star   <+> but_ <+> star

{-----------------------------------------------------------------------------
   Strong-weak patterns
------------------------------------------------------------------------------}

notJust1 :: Adjective -> Adjective -> Parser Text
notJust1 s w = not_ <+> word s <+> comma' <+> just_ <+> word w

notButJust :: Adjective -> Adjective -> Parser Text
notButJust s w = not_ <+> word s <+> comma' <+> but_ <+> just_ <+> word w

notStill :: Adjective -> Adjective -> Parser Text
notStill s w = not_ <+> word s <+> comma' <+> still_ <+> word w

notButStill :: Adjective -> Adjective -> Parser Text
notButStill s w = not_ <+> word s <+> comma' <+> but_ <+> still_ <+> word w

notAlthoughStill :: Adjective -> Adjective -> Parser Text
notAlthoughStill s w = not_ <+> word s <+> comma' <+> although_ <+> still_ <+> word w

notThoughStill :: Adjective -> Adjective -> Parser Text
notThoughStill s w = not_ <+> word s <+> comma' <+> though_ <+> still_ <+> word w

orVery :: Adjective -> Adjective -> Parser Text
orVery s w = word s <+> comma' <+> or_ <+>  very_ <+> word w

-- * wildcard patterns

notJust1' :: Parser Text
notJust1' = not_ <+> star <+> comma' <+> just_ <+> star

notButJust' :: Parser Text
notButJust' = not_ <+> star <+> comma' <+> but_ <+> just_ <+> star

notStill' :: Parser Text
notStill' = not_ <+> star <+> comma' <+> still_ <+> star

notButStill' :: Parser Text
notButStill' = not_ <+> star <+> comma' <+> but_ <+> still_ <+> star

notAlthoughStill' :: Parser Text
notAlthoughStill' = not_ <+> star <+> comma' <+> although_ <+> still_ <+> star

notThoughStill' :: Parser Text
notThoughStill' = not_ <+> star <+> comma' <+> though_ <+> still_ <+> star

orVery' :: Parser Text
orVery' = star <+> comma' <+> or_ <+>  very_ <+> star

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




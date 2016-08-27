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
   Pattern Lists
------------------------------------------------------------------------------}

p_weakStrong :: [(String, String -> String -> Parser Text)]
p_weakStrong = [ ("butNot"     , butNot     )
               , ("althoughNot", althoughNot)
               , ("thoughNot"  , thoughNot  )
               , ("andorEven"  , andorEven  )
               , ("andorAlmost", andorAlmost)
               , ("notOnly"    , notOnly    )
               , ("notJust"    , notJust    )
               ]


p_weakStrongStar :: [(String, Parser Text)]
p_weakStrongStar = [ ("butNot"      , butNot'     )
                   , ("althoughNot" , althoughNot')
                   , ("thoughNot"   , thoughNot'  )
                   , ("andorEven"   , andorEven'  )
                   , ("andorAlmost" , andorAlmost')
                   , ("notOnly"     , notOnly'    )
                   , ("notJust"     , notJust'    )
                   ]

p_strongWeak :: [(String, String -> String -> Parser Text)]                   
p_strongWeak = [ ("notJust1"        , notJust1        )
               , ("notButJust"      , notButJust      )
               , ("notStill"        , notStill        )
               , ("notButStill"     , notButStill     )
               , ("notAlthoughStill", notAlthoughStill)
               , ("notThoughStill"  , notThoughStill  )
               , ("orVery"          , orVery          )
               ]


p_strongWeakStar :: [(String, Parser Text)]
p_strongWeakStar = [ ("notJust1"        , notJust1'        )
                   , ("notButJust"      , notButJust'      )
                   , ("notStill"        , notStill'        )
                   , ("notButStill"     , notButStill'     )
                   , ("notAlthoughStill", notAlthoughStill')
                   , ("notThoughStill"  , notThoughStill'  )
                   , ("orVery"          , orVery'          )
                   ]

{-----------------------------------------------------------------------------
   Weak-strong patterns
------------------------------------------------------------------------------}


--    w (,) but not s
butNot :: String -> String -> Parser Text
butNot w s      = word w <+> butNot_ <+> word s

althoughNot :: String -> String -> Parser Text
althoughNot w s = word w <+> althoughNot_ <+> word s

thoughNot :: String -> String -> Parser Text
thoughNot w s   = word w <+> thoughNot_ <+> word s

andorEven :: String -> String -> Parser Text
andorEven w s   = word w <+> andorEven_ <+> word s

andorAlmost :: String -> String -> Parser Text
andorAlmost w s = word w <+> andorAlmost_ <+> word s

notOnly :: String -> String -> Parser Text
notOnly w s     = not_ <+> only_ <+> word w <+> but_ <+> word s

notJust :: String -> String -> Parser Text
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

notJust1 :: String -> String -> Parser Text
notJust1 s w = not_ <+> word s <+> comma' <+> just_ <+> word w

notButJust :: String -> String -> Parser Text
notButJust s w = not_ <+> word s <+> comma' <+> but_ <+> just_ <+> word w

notStill :: String -> String -> Parser Text
notStill s w = not_ <+> word s <+> comma' <+> still_ <+> word w

notButStill :: String -> String -> Parser Text
notButStill s w = not_ <+> word s <+> comma' <+> but_ <+> still_ <+> word w

notAlthoughStill :: String -> String -> Parser Text
notAlthoughStill s w = not_ <+> word s <+> comma' <+> although_ <+> still_ <+> word w

notThoughStill :: String -> String -> Parser Text
notThoughStill s w = not_ <+> word s <+> comma' <+> though_ <+> still_ <+> word w

orVery :: String -> String -> Parser Text
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
   Sords
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



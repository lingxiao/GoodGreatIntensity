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

import Control.Monad
import Control.Applicative

import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Text hiding (foldr)


{-----------------------------------------------------------------------------
   Run Parser
------------------------------------------------------------------------------}

infixr 9 <**
(<**) :: Parser a -> Text -> Maybe a
p <** t = case parse p t of
    Done _ r -> Just r
    _        -> Nothing


mParseOnly :: Parser a -> Text -> Maybe a
mParseOnly p t = case parseOnly p t of
  Right r -> Just r
  _       -> Nothing  

{-----------------------------------------------------------------------------
   Application specific parsers
------------------------------------------------------------------------------}


{-----------------------------------------------------------------------------
   Basic parsers
------------------------------------------------------------------------------}

comma :: Parser Text
comma = punct ','

-- * next char could either be a comma or zero or more spaces
commaS :: Parser Text
commaS = (const . pack $ "(,)") <$> (comma <|> spaces)

-- * parse zero or more spaces and ouput one space
spaces :: Parser Text
spaces = (const . pack $ " ") <$> many' space


{-----------------------------------------------------------------------------
  Utility
------------------------------------------------------------------------------}

-- * parse punctuation `p` either followed by zero or more spaces
-- * output "[p] "
punct :: Char -> Parser Text
punct p = (\_ _ -> pack $ [p] ++ " ") <$> (char p) <*> spaces





























-- * TODO: consider depricating this, since it's
-- * no longer needed since we have streaming abstraction


{-----------------------------------------------------------------------------
   Play with patterns here:

-- * search for first occurence of `w` from `vocab.txt` and output its count
cnt :: String -> Pattern
cnt w = vocabn w <|> vocab1 w

-- * hard wire one parser
-- butnot :: Pattern -> Pattern -> Pattern
-- butnot p1 p2 = do
--  w1 <- p1
--  return (w1, 0)

butNot :: Parser String
butNot = cat <$> string "good" <*> string " but not great"



-- * see if there's a way to represent this as a regular expression
{-
    p1 = "* (,) but not *"
-}

cat :: Text -> Text -> String
cat t1 t2 = unpack t1 ++ unpack t2


--aLine :: Parser String

{-----------------------------------------------------------------------------
   Parser utils 
------------------------------------------------------------------------------}


-- * Parse for word `w` in google 1gram vocab.txt
-- * file that appears in in the 1st line: "w\t..."
vocab1 :: String -> Pattern
vocab1 w = do
  string . pack $ w ++ "\t"
  n <- natural
  string "\n"
  return (w, read n)

-- * Parse for word `w` in google 1gram vocab.txt
-- * file that appears in some nth line:task  "...\nw\t..."
vocabn :: String -> Pattern
vocabn w = do
    manyTill anyChar . try . string . pack $ "\n" ++ w ++ "\t"
    n <- natural
    string "\n"
    return (w, read n)


-- * Parse a natural number
natural :: Parser String
natural = many1' digit

-- * skip over all words in Text stream until the word we want
pUntil :: String -> Parser String 
pUntil = manyTill anyChar . lookAhead . string . pack 

notSpace :: Parser Char
notSpace = notChar ' '


{-----------------------------------------------------------------------------
   adhoc tests
------------------------------------------------------------------------------}

-- * TODO: also need a parser that 
-- * will match the Psw and Psw patterns!!

-- * should succed
t1 = L.pack "hello\t999\nworld\t\900"
t2 = L.pack "world\t\900\nhello\t999\n"

-- * should fail
t3 = L.pack "world\t\900\nhello world\t999\n"
t4 = L.pack "hello world\t999\nworld\t\900"
t5 = L.pack "ahello\t999\nworld\t\900"
t6 = L.pack "ahello world\t999\nworld\t\900"
t7 = L.pack "world hello\t999\nworld\t\900"

w1 :: Pattern
w1 = cnt "hello"    

------------------------------------------------------------------------------}






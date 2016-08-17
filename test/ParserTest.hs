{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Test library of attoparsec parsers
-- | Author  : Xiao Ling
-- | Date    : 8/14/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module ParsersTest where

import Test.HUnit
import Data.Text hiding (foldr)
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator

import Parsers

{-----------------------------------------------------------------------------
   Run all tests
------------------------------------------------------------------------------}

main :: IO ()
main = do
    runTestTT . TestList 
              $ [ tspaces
                , tcomma
                , tcommaS

                ]
    return ()


{-----------------------------------------------------------------------------
    Test cases found in paper

    tintersperse :: Test
    tintersperse = "intersperse" ~:
    TestList [ intersperse ',' ""      ~?= "", 
         intersperse ',' "a"     ~?= "a", 
         intersperse ',' "abcde" ~?= "a,b,c,d,e" ]  


------------------------------------------------------------------------------}

tspaces :: Test
tspaces = "spaces" 
        ~: TestList [ mParseOnly spaces (pack ""   ) ~?= (Just . pack $ " ")
                    , mParseOnly spaces (pack "   ") ~?= (Just . pack $ " ")
                    , mParseOnly spaces (pack "hel") ~?= (Just . pack $ " ") 
                    ]

tcomma :: Test
tcomma = "comma" 
       ~: TestList [ mParseOnly comma (pack ", foo") ~?= (Just . pack $ ", ")
                   , mParseOnly comma (pack ",foo" ) ~?= (Just . pack $ ", ")
                   , mParseOnly comma (pack ",   foo")  
                                                     ~?= (Just . pack $ ", ")
                   , mParseOnly comma (pack "world") ~?=  Nothing
                   ]

tcommaS :: Test
tcommaS = let tok = pack "(,)" in "comma or space" 
        ~: TestList [ commaS <** pack ", foo" ~?= Just tok
                    , commaS <** pack " foo"  ~?= Just tok
                    , commaS <** pack "foo"   ~?= Nothing
                    ]






-- * patterns to be tested
t11 = pack "good and even great"
t12 = pack "good, and even great"
t13 = pack "aead, and even adfeafee"
t14 = pack "good or even great"
t15 = pack "good, or even great"
t16 = pack "aead, or even adfeafee"

t21 = pack "not only good but great"
t22 = pack "not only aereeds but afdf"


t31 = pack "not great just good"
t32 = pack "not great, just good"
t33 = pack "afda great just adfdfadf"
t34 = pack "afda great, just adfdfadf"



































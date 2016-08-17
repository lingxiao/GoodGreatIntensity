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
                , tanyWord

                , tbutnot
                ]
    return ()


pOnly :: Parser a -> Text -> Maybe a
pOnly p t = case parseOnly p t of
  Right r -> Just r
  _       -> Nothing  


{-----------------------------------------------------------------------------
    Basic parsers
------------------------------------------------------------------------------}

tspaces :: Test
tspaces = "spaces" 
        ~: TestList [ pOnly spaces (pack ""   ) ~?= (Just . pack $ " ")
                    , pOnly spaces (pack "   ") ~?= (Just . pack $ " ")
                    , pOnly spaces (pack "hel") ~?= (Just . pack $ " ") 
                    ]

tcomma :: Test
tcomma = "comma" 
       ~: TestList [ pOnly comma (pack ", foo"  ) ~?= (Just . pack $ ", ")
                   , pOnly comma (pack ",foo"   ) ~?= (Just . pack $ ", ")
                   , pOnly comma (pack ",   foo") ~?= (Just . pack $ ", ")
                   , pOnly comma (pack "world"  ) ~?=  Nothing
                   ]

tcommaS :: Test
tcommaS = let tok = pack "(,)" in "comma or space" 
        ~: TestList [ pOnly commaS (pack ", foo") ~?= Just tok
                    , pOnly commaS (pack " foo" ) ~?= Just tok
                    , pOnly commaS (pack "foo"  ) ~?= Nothing
                    ]


tanyWord :: Test
tanyWord = "anyWord"
        ~: TestList [ pOnly anyWord (pack "hello"      ) ~?= (Just . pack $ "hello")
                    , pOnly anyWord (pack "hello world") ~?= (Just . pack $ "hello")
                    , pOnly anyWord (pack "h"          ) ~?= (Just . pack $ "h"    )
                    , pOnly anyWord (pack "!"          ) ~?= Nothing
                    , pOnly anyWord (pack "9"          ) ~?= Nothing
                    , pOnly anyWord (pack "!hello"     ) ~?= Nothing
                    , pOnly anyWord (pack ""           ) ~?= Nothing
                    , pOnly anyWord (pack "    "       ) ~?= Nothing
                    ]


{-----------------------------------------------------------------------------
    Application specific parsers
------------------------------------------------------------------------------}


tbutnot :: Test
tbutnot =  let o = Just . pack $ "good (,) but not great"
        in "but not"
        ~: TestList [ "good" `butNot` "great" <** (pack "good but not great"   ) ~?= o
                    , "good" `butNot` "great" <** (pack "good, but not great"  ) ~?= o
                    , "good" `butNot` "great" <** (pack "good,   but not great") ~?= o
                    , "good" `butNot` "great" <** (pack "foo but not bar"      ) ~?= Nothing
                    , "good" `butNot` "great" <** (pack "foo,  but not bar"    ) ~?= Nothing
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



































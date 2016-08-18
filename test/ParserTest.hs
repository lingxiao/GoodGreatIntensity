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
                , tspaces1
                , tanyWord

                , tcomma
                , tcommaS
                , tbut

                , tbutnot
                , tbutnot'
                , talthoughnot
                ]
    return ()


pOnly :: Parser a -> Text -> Maybe a
pOnly p t = case parseOnly p t of
  Right r -> Just r
  _       -> Nothing  

justP :: String -> Maybe Text
justP = Just . pack

{-----------------------------------------------------------------------------
    Basic parsers
------------------------------------------------------------------------------}

tspaces :: Test
tspaces = "spaces" 
        ~: TestList [ pOnly spaces (pack ""   ) ~?= justP " "
                    , pOnly spaces (pack "   ") ~?= justP " "
                    , pOnly spaces (pack "hel") ~?= justP " "
                    ]

tspaces1 :: Test
tspaces1 = "spaces1" 
        ~: TestList [ pOnly spaces1 (pack ""   ) ~?= Nothing
                    , pOnly spaces1 (pack "   ") ~?= justP " "
                    , pOnly spaces1 (pack "hel") ~?= Nothing
                    ]

tanyWord :: Test
tanyWord = "anyWord"
        ~: TestList [ pOnly anyWord (pack "hello"      ) ~?= justP "hello"
                    , pOnly anyWord (pack "hello world") ~?= justP "hello"
                    , pOnly anyWord (pack "h"          ) ~?= justP "h"    
                    , pOnly anyWord (pack "!"          ) ~?= Nothing
                    , pOnly anyWord (pack "9"          ) ~?= Nothing
                    , pOnly anyWord (pack ""           ) ~?= Nothing
                    , pOnly anyWord (pack "    "       ) ~?= Nothing
                    ]

{-----------------------------------------------------------------------------
    Lexicon
------------------------------------------------------------------------------}

tcomma :: Test
tcomma = "comma" 
       ~: TestList [ pOnly comma (pack ","  ) ~?= justP ","
                   , pOnly comma (pack "  ,") ~?= justP ","
                   , pOnly comma (pack "h"  ) ~?= Nothing
                   ]


tcommaS :: Test
tcommaS = "commaS" 
       ~: TestList [ pOnly commaS (pack ","  ) ~?= justP "(,)"
                   , pOnly commaS (pack "  ,") ~?= justP "(,)"
                   , pOnly commaS (pack " ")   ~?= justP "(,)"
                   , pOnly commaS (pack "h"  ) ~?= Nothing
                   ]


tbut :: Test
tbut = "but_"
     ~: TestList [ but_ <** (pack "but"  )  ~?= justP "but"
                 , but_ <** (pack "  but")  ~?= justP "but"
                 , but_ <** (pack "  bbut") ~?= Nothing
                 ]


{-----------------------------------------------------------------------------
    Application specific parsers
------------------------------------------------------------------------------}

tbutnot :: Test
tbutnot =  let o = justP $ "good (,) but not great"
        in "but not"
        ~: TestList [ "good" `butNot` "great" <** (pack "good but not great"   ) ~?= o
                    , "good" `butNot` "great" <** (pack "good, but not great"  ) ~?= o
                    , "good" `butNot` "great" <** (pack "good ,  but not great") ~?= o
                    , "good" `butNot` "great" <** (pack "foo but not bar"      ) ~?= Nothing
                    , "good" `butNot` "great" <** (pack "foo,  but not bar"    ) ~?= Nothing
                    ]


tbutnot' :: Test
tbutnot' =  let o = justP $ "* (,) but not *"
        in "but not"
        ~: TestList [ pOnly butNot' (pack "foo but not bar" ) ~?= o
                    , pOnly butNot' (pack "goo but not gre" ) ~?= o
                    , pOnly butNot' (pack "foo, but not bar") ~?= o
                    , pOnly butNot' (pack "foo but yes bar" ) ~?= Nothing

                    ]


talthoughnot :: Test
talthoughnot =  let o = justP $ "good (,) although not great"
        in "but not"
        ~: TestList [ "good" `althoughNot` "great" <** (pack "good although not great"   ) ~?= o
                    , "good" `althoughNot` "great" <** (pack "good, although not great"  ) ~?= o
                    , "good" `althoughNot` "great" <** (pack "good ,  although not great") ~?= o
                    , "good" `althoughNot` "great" <** (pack "foo although not bar"      ) ~?= Nothing
                    , "good" `althoughNot` "great" <** (pack "foo,  although not bar"    ) ~?= Nothing
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



































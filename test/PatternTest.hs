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

module PatternTest where

import Test.HUnit
import Data.Text hiding (foldr)
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator

import Parsers
import Patterns

{-----------------------------------------------------------------------------
   Run all tests
------------------------------------------------------------------------------}

main :: IO ()
main = do
    runTestTT . TestList 
              $ [ tcomma
                , tcomma'
                , tbut

                , tbutnot
                , tbutnot'
                , talthoughnot
                ]
    return ()


justP :: String -> Maybe Text
justP = Just . pack

{-----------------------------------------------------------------------------
    words
------------------------------------------------------------------------------}

tcomma :: Test
tcomma = "comma" 
       ~: TestList [ comma <** (pack ","  ) ~?= justP ","
                   , comma <** (pack "  ,") ~?= justP ","
                   , comma <** (pack "h"  ) ~?= Nothing
                   ]


tcomma' :: Test
tcomma' = let o = justP "(,)"
       in "comma'" 
       ~: TestList [ comma' <** (pack ","  ) ~?= o
                   , comma' <** (pack "  ,") ~?= o
                   , comma' <** (pack " ")   ~?= o
                   , comma' <** (pack "h"  ) ~?= Nothing
                   ]


tbut :: Test
tbut = let o = justP "but"
    in "but_"
     ~: TestList [ but_ <** (pack "but"  )  ~?= o
                 , but_ <** (pack "  but")  ~?= o
                 , but_ <** (pack "  bbut") ~?= Nothing
                 ]



{-----------------------------------------------------------------------------
    Application specific parsers
------------------------------------------------------------------------------}

tbutnot :: Test
tbutnot =  let o = justP $ "good (,) but not great"
        in let p = (word "good") `butNot` (word "great")
        in "but not"
        ~: TestList [ p <** (pack "good but not great"        ) ~?= o
                    , p <** (pack "good, but not great"       ) ~?= o
                    , p <** (pack "good ,  but not great"     ) ~?= o
                    , p <** (pack "good but not great comment") ~?= o
                    , p <** (pack "foo but not bar"           ) ~?= Nothing
                    , p <** (pack "foo,  but not bar"         ) ~?= Nothing
                    ]


tbutnot' :: Test
tbutnot' =  let o = justP $ "* (,) but not *"
        in  let p = star `butNot` star
        in "* but not * "
        ~: TestList [ p <** (pack "foo but not bar" ) ~?= o
                    , p <** (pack "goo but not gre" ) ~?= o
                    , p <** (pack "foo, but not bar") ~?= o
                    , p <** (pack "foo but yes bar" ) ~?= Nothing

                    ]


talthoughnot :: Test
talthoughnot =  let o = justP $ "good (,) although not great"
             in let p = (word "good") `althoughNot` (word "great")
             in "although not"
        ~: TestList [ p <** (pack "good although not great"   ) ~?= o
                    , p <** (pack "good, although not great"  ) ~?= o
                    , p <** (pack "good ,  although not great") ~?= o
                    , p <** (pack "foo although not bar"      ) ~?= Nothing
                    , p <** (pack "foo,  although not bar"    ) ~?= Nothing
                    ]




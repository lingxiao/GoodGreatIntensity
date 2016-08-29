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


right :: String -> Either String Text
right = Right . pack

name' :: Show a => Parser a -> Either String a
name' = Left . name

{-----------------------------------------------------------------------------
    words
------------------------------------------------------------------------------}

tcomma :: Test
tcomma = "comma" 
       ~: TestList [ comma <** (pack ","  ) ~?= right ","
                   , comma <** (pack "  ,") ~?= right ","
                   , comma <** (pack "h"  ) ~?= name' comma
                   ]


tcomma' :: Test
tcomma' = let o = right "(,)"
       in "comma'" 
       ~: TestList [ comma' <** (pack ","  ) ~?= o
                   , comma' <** (pack "  ,") ~?= o
                   , comma' <** (pack " ")   ~?= o
                   , comma' <** (pack "h"  ) ~?= name' comma'
                   ]


tbut :: Test
tbut = let o = right "but"
    in "but_"
     ~: TestList [ but_ <** (pack "but"  )  ~?= o
                 , but_ <** (pack "  but")  ~?= o
                 , but_ <** (pack "  bbut") ~?= name' but_
                 ]



{-----------------------------------------------------------------------------
    Application specific parsers
------------------------------------------------------------------------------}

tbutnot :: Test
tbutnot =  let o = right $ "good (,) but not great"
        in let p = (word "good") `butNot` (word "great")
        in "but not"
        ~: TestList [ p <** (pack "good but not great"        ) ~?= o
                    , p <** (pack "good, but not great"       ) ~?= o
                    , p <** (pack "good ,  but not great"     ) ~?= o
                    , p <** (pack "good but not great comment") ~?= o
                    , p <** (pack "foo but not bar"           ) ~?= name' p
                    , p <** (pack "foo,  but not bar"         ) ~?= name' p
                    ]


tbutnot' :: Test
tbutnot' =  let o = right $ "* (,) but not *"
        in  let p = star `butNot` star
        in "* but not * "
        ~: TestList [ p <** (pack "foo but not bar" ) ~?= o
                    , p <** (pack "goo but not gre" ) ~?= o
                    , p <** (pack "foo, but not bar") ~?= o
                    , p <** (pack "foo but yes bar" ) ~?= name' p

                    ]


talthoughnot :: Test
talthoughnot =  let o = right $ "good (,) although not great"
             in let p = (word "good") `althoughNot` (word "great")
             in "although not"
        ~: TestList [ p <** (pack "good although not great"   ) ~?= o
                    , p <** (pack "good, although not great"  ) ~?= o
                    , p <** (pack "good ,  although not great") ~?= o
                    --, p <** (pack "foo although not bar"      ) ~?= name' p
                    --, p <** (pack "foo,  although not bar"    ) ~?= name' p
                    ]




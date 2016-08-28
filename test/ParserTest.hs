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
                , tNotAlphaDigitSpace
                , teow
                , tword
                , tanyWord
                ]
    return ()



justP :: String -> Maybe Text
justP = Just . pack

{-----------------------------------------------------------------------------
    Basic parsers
------------------------------------------------------------------------------}

tspaces :: Test
tspaces = "spaces" 
        ~: TestList [ spaces <** (pack ""   ) ~?= justP " "
                    , spaces <** (pack "   ") ~?= justP " "
                    , spaces <** (pack "hel") ~?= justP " "
                    ]

tspaces1 :: Test
tspaces1 = "spaces1" 
        ~: TestList [ spaces1<**  (pack ""   ) ~?= Nothing
                    , spaces1<**  (pack "   ") ~?= justP " "
                    , spaces1<**  (pack "hel") ~?= Nothing
                    ]


tNotAlphaDigitSpace :: Test
tNotAlphaDigitSpace = "notAlphaDigitSpace"
     ~: TestList [ notAlphaDigitSpace <** (pack ".") ~?= Just '.'
                 , notAlphaDigitSpace <** (pack " ") ~?= Nothing
                 , notAlphaDigitSpace <** (pack "1") ~?= Nothing
                 , notAlphaDigitSpace <** (pack "h") ~?= Nothing
     ]


teow :: Test
teow = "eow"
    ~: TestList [ eow <** (pack "..."  ) ~?= justP "..."
                , eow <** (pack ".. "  ) ~?= justP ".."
                , eow <** (pack ".. hi") ~?= justP ".."

                , eow <** (pack "..1"  ) ~?= Nothing
                , eow <** (pack ".h" )   ~?= Nothing
                , eow <** (pack "..h")   ~?= Nothing
    ]


tword :: Test
tword =  let p = word  "hello"
      in let o = justP "hello"
      in "word"                    
      ~: TestList [ p <** (pack "hello"  ) ~?= o
                  , p <** (pack "hello!" ) ~?= o
                  , p <** (pack "  hello") ~?= o
                  , p <** (pack "hello " ) ~?= o
                  , p <** (pack "foo"    ) ~?= Nothing
                  , p <** (pack "hello1" ) ~?= Nothing
                  , p <** (pack "helloo" ) ~?= Nothing

                  , p <** (pack "hello..."    ) ~?= o
                  , p <** (pack "hello.f"     ) ~?= Nothing
                  , p <** (pack "hello......f") ~?= Nothing
                  , p <** (pack "hello-/hello") ~?= Nothing
                  , p <** (pack "hello'ol"    ) ~?= Nothing
                  , p <** (pack "hello.com"   ) ~?= Nothing
                  , p <** (pack "hello.kw.net") ~?= Nothing
                  ]

tanyWord :: Test
tanyWord = let o = justP "hello" 
        in "anyWord"
        ~: TestList [ anyWord <** (pack "hello"      ) ~?= o
                    , anyWord <** (pack "hello world") ~?= o
                    , anyWord <** (pack "hello..."   ) ~?= o
                    , anyWord <** (pack "h"          ) ~?= justP "h"    

                    , anyWord <** (pack "!"          ) ~?= Nothing
                    , anyWord <** (pack "9"          ) ~?= Nothing
                    , anyWord <** (pack ""           ) ~?= Nothing
                    , anyWord <** (pack "    "       ) ~?= Nothing

                    , anyWord <** (pack "good.f"     ) ~?= Nothing
                    , anyWord <** (pack "good.....f" ) ~?= Nothing
                    , anyWord <** (pack "good-/good" ) ~?= Nothing
                    , anyWord <** (pack "good'ol"    ) ~?= Nothing
                    , anyWord <** (pack "good.com"   ) ~?= Nothing
                    ]


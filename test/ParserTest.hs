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
import Patterns

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

                , tcomma
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
                  ]

tanyWord :: Test
tanyWord = let o = justP "hello" 
        in "anyWord"
        ~: TestList [ anyWord <** (pack "hello"      ) ~?= justP "hello"
                    , anyWord <** (pack "hello world") ~?= justP "hello"
                    , anyWord <** (pack "h"          ) ~?= justP "h"    
                    , anyWord <** (pack "!"          ) ~?= Nothing
                    , anyWord <** (pack "9"          ) ~?= Nothing
                    , anyWord <** (pack ""           ) ~?= Nothing
                    , anyWord <** (pack "    "       ) ~?= Nothing

                    , anyWord <** (pack "hello..."    ) ~?= o
                    , anyWord <** (pack "hello.f"     ) ~?= Nothing
                    , anyWord <** (pack "hello......f") ~?= Nothing
                    , anyWord <** (pack "hello-/hello") ~?= Nothing
                    , anyWord <** (pack "hello'ol"    ) ~?= Nothing
                    , anyWord <** (pack "hello.com"   ) ~?= Nothing
                    ]

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
        in let p = "good" `butNot` "great" 
        in "but not"
        ~: TestList [ p <** (pack "good but not great"   ) ~?= o
                    , p <** (pack "good, but not great"  ) ~?= o
                    , p <** (pack "good ,  but not great") ~?= o
                    , p <** (pack "foo but not bar"      ) ~?= Nothing
                    , p <** (pack "foo,  but not bar"    ) ~?= Nothing
                    ]


tbutnot' :: Test
tbutnot' =  let o = justP $ "* (,) but not *"
        in "but not"
        ~: TestList [ butNot' <** (pack "foo but not bar" ) ~?= o
                    , butNot' <** (pack "goo but not gre" ) ~?= o
                    , butNot' <** (pack "foo, but not bar") ~?= o
                    , butNot' <** (pack "foo but yes bar" ) ~?= Nothing

                    ]


talthoughnot :: Test
talthoughnot =  let o = justP $ "good (,) although not great"
             in let p = "good" `althoughNot` "great"
             in "but not"
        ~: TestList [ p <** (pack "good although not great"   ) ~?= o
                    , p <** (pack "good, although not great"  ) ~?= o
                    , p <** (pack "good ,  although not great") ~?= o
                    , p <** (pack "foo although not bar"      ) ~?= Nothing
                    , p <** (pack "foo,  although not bar"    ) ~?= Nothing
                    ]




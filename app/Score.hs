{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : All functions used to find score
-- | Author  : Xiao Ling
-- | Date    : 8/17/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Score where

import Prelude hiding (filter)

import System.Directory
import System.FilePath.Posix
import qualified System.IO as S

import Control.Monad.State  
import Control.Monad.IO.Class 
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader

import Data.Conduit 
import Conduit (mapC, scanlC, foldlC, filterC)
import Data.Text hiding (foldr, length)
import Data.Attoparsec.Text

import Core
import Parsers
import Patterns
import Conduits
import Preprocess



{-----------------------------------------------------------------------------
  Type
------------------------------------------------------------------------------}

type ParseResult = (Text,Text,Integer)
type Total       = Integer
type OutPath     = FilePath
data Sys         = S { out :: FilePath, onegm :: FilePath, ngm :: [FilePath]}
  deriving (Show)

{----------------------------------------------------------------------------
  Score 
-----------------------------------------------------------------------------}


-- * TODO: this should be readerT instance  

score :: (Show a, Fractional a) => Adjective -> Adjective -> ReaderT Sys IO a
score a1 a2 = do

  -- * compute score
  p1' <- fromInteger <$> sumcnt (p_weakStrong "*" "*")
  p2' <- fromInteger <$> sumcnt (p_strongWeak "*" "*")

  w1' <- fromInteger <$> w1 a1 a2
  w2' <- fromInteger <$> w2 a2 a2
  
  s1' <- fromInteger <$> s1 a1 a2
  s2' <- fromInteger <$> s2 a1 a2

  let w1'' = w1' / p1'
  let w2'' = w2' / p1'
  let s1'' = s1' / p2'
  let s2'' = s2' / p2'


  a1' <- fromInteger <$> cntwd (a1, word a1)
  a2' <- fromInteger <$> cntwd (a2, word a2)

  let top = (w1'' - s1'') - (w2'' - s2')
  let bot = a1' * a2'

  let score' = top / bot

  -- * save file
  sys <- ask
  let xs1 = "(" ++ show w1'' ++ " - " ++ show s1'' ++ ")"
  let xs2 = "(" ++ show w2'' ++ " - " ++ show s2'' ++ ")"
  let xs3 = show a1' ++ " * " ++ show a2'

  let eq = show score' ++ " = "
          ++ xs1 ++ " - " ++ xs2 ++ "  /  " ++ xs3

  let name = out sys ++ "/" ++ a1 ++ "_" ++ a2

  liftIO $ writeScore name eq score'

  return score'


{-----------------------------------------------------------------------------
  Score parts
------------------------------------------------------------------------------}

w1 :: Adjective -> Adjective -> ReaderT Sys IO Total
w1 a1 = sumcnt . p_weakStrong a1

w2 :: Adjective -> Adjective -> ReaderT Sys IO Total
w2 a1 a2 = w1 a2 a1

s1 :: Adjective -> Adjective -> ReaderT Sys IO Total
s1 a1 = sumcnt . p_strongWeak a1

s2 :: Adjective -> Adjective -> ReaderT Sys IO Total
s2 a1 a2 = s1 a2 a1


{-----------------------------------------------------------------------------
  Score subroutines
------------------------------------------------------------------------------}

-- * count all occurences of `Pattern` 
-- * and save the cumulative results, also save intermediate results
sumcnt :: (Name, [Pattern]) -> ReaderT Sys IO Total
sumcnt (name, ps) = do
  sys <- ask
  ns  <- mapM cnt ps
  let m = sum ns
  liftIO $ writeResult (out sys ++ "/" ++ name ++ ".txt") m []
  return m


-- * Given `Pattern` named `name` and parser `p`,
-- * query all `ngmp`ath for occurences of pattern
cnt :: Pattern -> ReaderT Sys IO Total
cnt p = querySave p ngm

-- * Given `Pattern` named `name` and parser `p`,
-- * query `onegm` for occurences of pattern
cntwd :: Pattern -> ReaderT Sys IO Total
cntwd p = querySave p (\s -> [onegm s])


-- * `query` file, print results, and save results
querySave :: Pattern -> (Sys -> [FilePath]) -> ReaderT Sys IO Total
querySave (name, p) g = do
  sys     <- ask
  let (outp, fs) = (out sys, g sys)
  (n, ts) <- query fs p
  liftIO $ writeResult (outp ++ "/" ++ name) n ts

  liftIO $ print name
  liftIO $ print "================================="
  liftIO $ mapM (\(w,w',n) ->  print
                  $  unpack w 
                  ++ "     " 
                  ++ unpack w' 
                  ++ "     "
                  ++ show n) ts
    
  return n

{-----------------------------------------------------------------------------
  Conduit routines
------------------------------------------------------------------------------}

-- * `query` for occurences of `p` in all files found at paths `fs`
query :: Op m 
      => [FilePath] -> Parser Text -> m (Total,[ParseResult])
query fs p  = eval $ streamFile fs $$ queryFile p

-- * open all ".txt" files found at path `p` and stream them as lines
-- * preprocess each line by casefolding and stripping of whitespace
streamFile :: FileOpS m s => [FilePath] -> Source m ParseResult
streamFile fs =  fs `sourceDirectories` ".txt"
             =$= openFile
             =$= linesOn "\t"
             =$= filterC (\x -> Prelude.length x == 2)
             =$= mapC    (\[w,n] -> (pre w, w, read . unpack $ n :: Integer))

       
-- * search for pattern `p` and sum all of its occurences
-- * save occurences in local state
queryFile :: FileOpS m [ParseResult]
           => Parser Text 
           -> Consumer ParseResult m Total
queryFile p =  filterC (\(w,_,_) -> 
                        if p <** w == Nothing then False 
                                              else True)
            -- * =$= logi
            =$= awaitForever (\t -> do
                    ts <- lift get
                    let ts' = t:ts
                    lift . put $ ts'
                    yield t
                )
            =$= foldlC  (\m (_,_,n) -> m + n) 0


{-----------------------------------------------------------------------------
  Save file
------------------------------------------------------------------------------}

-- * write result named `name` to local directory,
-- * result is total count `n` and incidences occured `xs`
writeResult :: String -> Integer -> [ParseResult] -> IO ()
writeResult xs n ts = do
    let ys   = dropExtension xs
    let file = ys ++ ".txt"
    let name = takeFileName file
    o <- S.openFile file S.WriteMode
    S.hPutStrLn o name
    S.hPutStrLn o mark
    S.hPutStrLn o $ "total: " ++ show n
    S.hPutStrLn o mark
    mapM (\(w,w',n) ->  S.hPutStrLn o 
                     $  unpack w 
                     ++ "     " 
                     ++ unpack w' 
                     ++ "     "
                     ++ show n) ts
    S.hClose o
    return ()
        where mark = foldr (++) mempty $ (const "-") <$> [1..50] 



-- * write result named `name` to this directory,
-- * result is score equation `eq` and actual score `n`

writeScore :: Show a => FilePath -> String -> a -> IO ()
writeScore xs eq n = do
    let ys   = dropExtension xs
    let file = ys ++ ".txt"
    let name = takeFileName file
    o <- S.openFile file S.WriteMode
    S.hPutStrLn o name
    S.hPutStrLn o mark
    S.hPutStrLn o $ "score: " ++ show n
    S.hPutStrLn o mark
    S.hPutStrLn o eq
    S.hClose o
    return ()
        where mark = foldr (++) mempty $ (const "-") <$> [1..50] 









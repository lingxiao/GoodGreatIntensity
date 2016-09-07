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

module Score (

    Sys (..)
  , w1
  , w2
  , s1
  , s2
  , sumCount
  , countWord

  ) where

import Prelude hiding (filter)

import System.Directory
import System.FilePath.Posix
import qualified System.IO as S

import Control.Monad.State  
import Control.Monad.IO.Class 
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader

import Data.Conduit 
import Data.Text            hiding (foldr, length, count)
import Data.Attoparsec.Text hiding (count)
import Conduit              (mapC, scanlC, foldlC, filterC)

import Core
import Parsers
import Patterns
import Conduits
import Preprocess


{-----------------------------------------------------------------------------
  Type
------------------------------------------------------------------------------}

type ParseResult = (Text,Text,Integer)
type OutPath     = FilePath
data Sys         = S { out :: FilePath, onegram :: FilePath, ngrams :: [FilePath]}
                       deriving (Show)

{-----------------------------------------------------------------------------
  Score 
------------------------------------------------------------------------------}

w1 :: Adjective -> Adjective -> ReaderT Sys IO Integer
w1 a1 a2 = sumCount $ p_weakStrong (word a1) (word a2)

s1 :: Adjective -> Adjective -> ReaderT Sys IO Integer
s1 a1 a2 = sumCount $ p_strongWeak (word a1) (word a2)

w2 :: Adjective -> Adjective -> ReaderT Sys IO Integer
w2 = flip w1

s2 :: Adjective -> Adjective -> ReaderT Sys IO Integer
s2 = flip s1

{-----------------------------------------------------------------------------
  Score subroutines
------------------------------------------------------------------------------}


-- * Given parser `p`, query `onegram` for occurences of `p`
countWord :: Parser Text -> ReaderT Sys IO Integer
countWord p = querySave p (\s -> [onegram s])


-- * count all occurences of `Pattern` 
-- * and save the cumulative results, also save intermediate results
sumCount :: (Name, [Parser Text]) -> ReaderT Sys IO Integer
sumCount (name, ps) = do
  sys <- ask
  ns  <- mapM count ps
  let m = sum ns
  liftIO $ writeResult (out sys ++ "/" ++ name ++ ".txt") m []
  return m

-- * Given `Pattern` named `name` and parser `p`,
-- * query all `ngramsp`ath for occurences of pattern
count :: Parser Text -> ReaderT Sys IO Integer
count = flip querySave ngrams

-- * given a parser `p` and function
-- * to access the `files` in `sys`tem
-- * `query` file, print results, and `save` results
-- * int `out`put directory specified by `sys`tem
querySave :: Parser Text 
          -> (Sys -> [FilePath]) 
          -> ReaderT Sys IO Integer
querySave p files = do
  sys     <- ask
  let (outp, fs) = (out sys, files sys)
  (n, ts) <- query fs p
  liftIO $ writeResult (outp ++ "/" ++ name p) n ts

  liftIO $ print $ name p ++ "  " ++ show n
  liftIO $ print "================================="
  liftIO $ mapM (\(w,w',n) ->  print
                  $  unpack w 
                  ++ "     " 
                  ++ unpack w' 
                  ++ "     "
                  ++ show n
                  ++ "\n") ts
    
  return $ fromInteger n

{-----------------------------------------------------------------------------
  Conduit routines
------------------------------------------------------------------------------}

-- * `query` for occurences of `p` in all files found at paths `fs`
query :: (Op m , Fractional a)
      => [FilePath] -> Parser Text -> m (Integer,[ParseResult])
query fs p  = eval $ openFiles fs $$ queryFiles p

-- * open all ".txt" files found at path `p` and stream them as lines
-- * preprocess each line by casefolding and stripping of whitespace
openFiles :: FileOpS m s => [FilePath] -> Source m ParseResult
openFiles fs =  fs `sourceDirectories` ".txt"
             =$= openFile
             =$= linesOn "\t"
             =$= filterC (\x     -> Prelude.length x == 2)
             =$= mapC    (\[w,n] -> (pre w, w, read . unpack $ n :: Integer))

-- * search for pattern `p` and sum all of its occurences
-- * save occurences in local state
queryFiles :: FileOpS m [ParseResult]
           => Parser Text 
           -> Consumer ParseResult m Integer
queryFiles p =  filterC (\(w,_,_) -> case p <** w of
                        Left _ -> False
                        _      -> True)
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
writeResult :: String 
            -> Integer
            -> [ParseResult] 
            -> IO ()
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



{----------------------------------------------------------------------------
  @Depricated. Score 

-- * do not actually run this
score :: (Show a, Fractional a) 
      => Adjective 
      -> Adjective 
      -> ReaderT Sys IO a
score a1 a2 = do

  -- * compute score
  p1' <- fromInteger <$> sumcount (p_weakStrong star star)
  p2' <- fromInteger <$> sumcount (p_strongWeak star star)

  w1' <- fromInteger <$> w1 a1 a2
  w2' <- fromInteger <$> w2 a1 a2
  
  s1' <- fromInteger <$> s1 a1 a2
  s2' <- fromInteger <$> s2 a1 a2

  let w1'' = w1' / p1'
  let w2'' = w2' / p1'
  let s1'' = s1' / p2'
  let s2'' = s2' / p2'

  na1 <- fromInteger <$> (countwd $ word a1)
  na2 <- fromInteger <$> (countwd $ word a2)

  let top = (w1'' - s1'') - (w2'' - s2')
  let bot = na1 * na2

  let score' = top / bot

  sys <- ask

  -- * print save message
  let xs1 = "(" ++ show w1'' ++ " - " ++ show s1'' ++ ")"
  let xs2 = "(" ++ show w2'' ++ " - " ++ show s2'' ++ ")"
  let xs3 = show na1 ++ " * " ++ show na2

  let eq = show score' ++ " = "
          ++ xs1 ++ " - " ++ xs2 ++ "  /  " ++ xs3

  let name = out sys ++ "/" ++ a1 ++ "_" ++ a2

  -- * save file
  liftIO $ writeScore name eq score'

  return score'
-----------------------------------------------------------------------------}





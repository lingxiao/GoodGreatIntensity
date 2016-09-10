{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Exposes two solutions to query ngrams on disk
-- | Author  : Xiao Ling
-- | Date    : 9/10/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module System (
      query
    , query'
    , pattern
  ) where


import System.Directory
import System.FilePath.Posix
import qualified System.IO as S

import Control.Monad.State  
import Control.Monad.Trans.Reader

import Conduit              (mapC, scanlC, foldlC, filterC)

import Data.Conduit 
import Data.List.Split      (splitOn)
import Data.Text            (Text, unpack, pack)
import Data.Attoparsec.Text hiding (count)


import Core
import Conduits
import Preprocess
import PatternCompiler


{-----------------------------------------------------------------------------
  Open pattern
------------------------------------------------------------------------------}

-- * `get` pattern path and open, then compile
pattern :: (Config -> FilePath) -> ReaderT Config IO [Pattern]
pattern get = do
  con <- ask
  ps  <- liftIO $ S.readFile (get con) 
  let ps' = lines ps
  return $ compile <$> ps'

{-----------------------------------------------------------------------------
  Query using non-list-streaming solution
------------------------------------------------------------------------------}

-- * given *directory paths* `ds`, and parser `p`
-- * `queryAll` occurences of strings recognized by `p`
-- * and sum results
query :: MonadTrans m => Parser Text -> [FilePath] -> m IO Output
query p = lift . queryio p

-- * given *directory paths* `ds`, and parser `p`
-- * `queryAll` occurences of strings recognized by `p`
-- * and sum results
queryio :: Parser Text -> [FilePath] -> IO Output
queryio p ds = sourceDirs ".txt" ds >>= queryAll p


-- * Given path to ".txt" files `fs` and parser `p`,
-- * `queryOne` each ".txt" files and sum the results of
-- * the queries
queryAll :: Parser Text -> [FilePath] -> IO Output
queryAll p fs = do
  let os = queryOne p <$> fs
  rrs    <- sequence os
  let rs = foldr (\(n,q) (m,qs) -> (n+m,q++qs)) (0,[]) rrs
  return rs

-- * given parser `p`, query the ".txt" file `f`
-- * for occurences of string recognized by `p`
queryOne :: Parser Text -> FilePath -> IO Output
queryOne p f = do
  ys <- splitOn "\n"     <$> readFile f
  let yys  = splitOn "\t" <$> ys
  let yys' = filter (\ys -> length ys == 2) yys
  let xs   = (\[y,n] -> (preprocess . pack $ y, pack y, read n)) 
          <$> yys'
  let rs   = filter (matchP p) xs
  let n    = foldr (\(_,_,n) m -> m + n) 0 rs
  return (n,rs)

-- * check if text `t` is recognized by `p`
matchP :: Parser Text -> QueryResult -> Bool
matchP p (t,_,_) = case p <** t of
  Right _ -> True
  _       -> False


-- * Given file paths `fs` and file extension `ext`
-- * list all files in directories with this extension
sourceDirs :: String -> [FilePath] -> IO [FilePath]
sourceDirs ext fs = do
  dds <- sequence $  sourceDir ext <$> fs
  return $ concat dds


-- * Given directory path `d` and file extension `ext`
-- * list all files in directory with this extension
sourceDir :: String -> FilePath -> IO [FilePath]
sourceDir ext d = do
  fs <- getDirectoryContents d
  let fs' = filter (\f -> takeExtension f == ext) fs
  return $ (\f -> d ++ "/" ++ f) <$> fs'



{-----------------------------------------------------------------------------
  Query using list streaming solution build from Data.Conduit
------------------------------------------------------------------------------}

-- * `query` for occurences of utterance to be parsed by parser `p` 
-- * in all files found at paths `fs`
query' :: (Op m , Fractional a)
      => Parser Text -> [FilePath] ->  m Output
query' p fs  = eval $ openFiles fs $$ queryFiles p

-- * open all ".txt" files found at paths `fs` and stream them as lines
-- * preprocess each line by casefolding and stripping of whitespace
openFiles :: FileOpS m s => [FilePath] -> Source m QueryResult
openFiles fs =  fs `sourceDirectories` ".txt"
             =$= openFile
             =$= linesOn "\t"
             =$= filterC (\x     -> Prelude.length x == 2)
             =$= mapC    (\[xs,n] -> ( preprocess xs
                                    , xs
                                    , read . unpack $ n :: Integer))

-- * search for pattern parsed by parser `p` and 
-- * sum all of its occurences
queryFiles :: FileOpS m [QueryResult]
           => Parser Text 
           -> Consumer QueryResult m Integer
queryFiles p =  filterC (\(xs,_,_) -> case p <** xs of
                        Left _ -> False
                        _      -> True)
            =$= awaitForever (\t -> do
                    ts <- lift get
                    let ts' = t:ts
                    lift . put $ ts'
                    yield t
                )
            =$= foldlC  (\m (_,_,n) -> m + n) 0




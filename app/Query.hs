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

module Query (
    query
  , query'
  , pattern
  ) where


import System.Directory
import System.FilePath.Posix
import qualified System.IO as S

import Control.Monad.State  
import Control.Monad.Trans.Reader

import Data.Conduit 
import Data.Text            (Text, unpack, pack, splitOn)
import Data.Attoparsec.Text hiding (count)
import Conduit              (mapC, scanlC, foldlC, filterC)

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
query :: MonadTrans t 
      => Parser Text 
      -> [DirectoryPath] 
      -> t IO Output
query p ds = lift $ do
  ts <- openTxtFiles ds
  return $ p `queryFile` ts

-- * given directory paths `ds`
-- * open all text files and concat results
openTxtFiles :: [DirectoryPath] -> IO Text
openTxtFiles ds = do
  fs   <- sourceDirs ".txt" ds
  file <- sequence $ readFile <$> fs
  return . pack . concat $ file

-- * Given text file `f`, query for occurences of 
-- * string recognized by `p`
queryFile :: Parser Text -> Text -> Output
queryFile p ts = (n, rs)
  where
    ys   = splitOn (pack "\n") ts
    yys  = splitOn (pack "\t") <$> ys
    yys' = filter (\ys -> length ys == 2) yys
    xs   = (\[y,n] -> (preprocess y, y, read . unpack $ n)) <$> yys'
    rs   = p `matchLoop` xs
    n    = foldr (\(_,_,n) m -> n + m) 0 rs

-- * loop through all files and check if text `t` is 
-- * recognzied by parser `p`, if so then put into stack  
matchLoop :: Parser Text -> [QueryResult] -> [QueryResult]
matchLoop p = filter (match p) where
    match p (t,_,_) = case p <** t of
        Right _ -> True
        _       -> False


{-----------------------------------------------------------------------------
  Query using list streaming solution build from Data.Conduit
------------------------------------------------------------------------------}

-- * `query` for occurences of utterance to be parsed by parser `p` 
-- * in all files found at paths `fs`
query' :: (Op m , Fractional a)
      => Parser Text -> [FilePath] ->  m Output
query' p fs  = eval $ openTxtFiles' fs $$ queryFile' p

-- * open all ".txt" files found at directories `fs` and stream them as lines
-- * preprocess each line by casefolding and stripping of whitespace
openTxtFiles' :: FileOpS m s => [DirectoryPath] -> Source m QueryResult
openTxtFiles' fs =  fs `sourceDirectories` ".txt"
             =$= openFile
             =$= linesOn "\t"
             =$= filterC (\x     -> Prelude.length x == 2)
             =$= mapC    (\[xs,n] -> ( preprocess xs
                                    , xs
                                    , read . unpack $ n :: Integer))

-- * search for pattern parsed by parser `p` and 
-- * sum all of its occurences
queryFile' :: FileOpS m [QueryResult]
           => Parser Text 
           -> Consumer QueryResult m Integer
queryFile' p =  filterC (\(xs,_,_) -> case p <** xs of
                        Left _ -> False
                        _      -> True)
            =$= awaitForever (\t -> do
                    ts <- lift get
                    let ts' = t:ts
                    lift . put $ ts'
                    yield t
                )
            =$= foldlC  (\m (_,_,n) -> m + n) 0


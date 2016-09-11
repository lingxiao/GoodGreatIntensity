{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Shared datatypes and IO functions of this application
-- | Author  : Xiao Ling
-- | Date    : 9/8/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Core (

    Output
  , QueryResult
  , DirectoryPath
  , Config (..)

  , sourceDirs
  , sourceDir
  , makeDirAtTop
  , writeDummyTo
  , writeOutput

  ) where 

import System.Directory
import System.FilePath.Posix
import qualified System.IO as S

import Data.Time.Clock
import Data.Text (Text, unpack)
import Data.List.Split (splitOn)
import Data.Attoparsec.Text 


{-----------------------------------------------------------------------------
  Output of query ngrams
------------------------------------------------------------------------------}

-- * Results from a single `Query` to ngram documents
-- * the first field is preprocess text, second field is orginal text
-- * the third field is number of results
type QueryResult  = (Text,Text,Integer)
type Output       = (Integer, [QueryResult])

type DirectoryPath = FilePath

{-----------------------------------------------------------------------------
    Filepath configuration
------------------------------------------------------------------------------}

data Config = Con {
      onegram    :: FilePath
    , ngrams     :: [FilePath]
    , strongWeak :: FilePath
    , weakStrong :: FilePath
} deriving (Show, Eq)

{-----------------------------------------------------------------------------
  Read from Disk
------------------------------------------------------------------------------}

-- * Given directory paths `ds` and file extension `ext`
-- * list all files in directories with this extension
sourceDirs :: String -> [FilePath] -> IO [FilePath]
sourceDirs ext ds = do
  dds <- sequence $  sourceDir ext <$> ds
  return $ concat dds


-- * Given directory path `d` and file extension `ext`
-- * list all files in directory with this extension
sourceDir :: String -> FilePath -> IO [FilePath]
sourceDir ext d = do
  fs      <- getDirectoryContents d
  let fs' = filter (\f -> takeExtension f == ext) fs
  return $ (\f -> d ++ "/" ++ f) <$> fs'


{-----------------------------------------------------------------------------
  Write to Disk
------------------------------------------------------------------------------}

-- * create directory `f` at top of project folder
makeDirAtTop :: FilePath -> IO FilePath
makeDirAtTop f = do
      xs <- getCurrentDirectory
      let project = "GoodGreatIntensity"
      let top:_   = splitOn project xs
      let dir     = top ++ project ++ "/" ++ takeBaseName f
      createDirectoryIfMissing False dir
      return dir

-- * given new folder name `f`, 
-- * create folder at $HOME/Project and 
-- * save a dummy file for test purposes
writeDummyTo :: FilePath -> IO ()      
writeDummyTo f = do
    outdir   <- makeDirAtTop f
    let tf = outdir ++ "/dummyTest.txt"
    writeOutput tf (404,[])

writeOutput :: FilePath -> Output -> IO ()
writeOutput f (n, rs) = do
  let name = takeFileName . dropExtension $ f
  h    <- S.openFile f S.WriteMode
  time <- show <$> getCurrentTime

  S.hPutStrLn h name
  S.hPutStrLn h time

  S.hPutStrLn h mark
  S.hPutStrLn h $ "total: " ++ show n
  S.hPutStrLn h mark
  
  mapM (\(w,w',n) ->  S.hPutStrLn h
                   $  unpack w 
                   ++ "     " 
                   ++ unpack w' 
                   ++ "     "
                   ++ show n) rs

  S.hClose h
  return ()
      where mark = foldr (++) mempty $ (const "-") <$> [1..50] 



      


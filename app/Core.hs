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

module Core where 

import System.Directory
import System.FilePath.Posix
import qualified System.IO as S

import Data.Text (Text, unpack)
import Data.List.Split (splitOn)
import Data.Attoparsec.Text 



{-----------------------------------------------------------------------------
  Output of query ngrams
------------------------------------------------------------------------------}

-- * Results from a single `Query` to ngram documents
-- * the first field is preprocess text, second field is orginal text
-- * the third field is number of results
type QueryResult = (Text,Text,Integer)
type Output      = (Integer, [QueryResult])


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
  Write
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


saveOutput :: FilePath -> Output -> IO ()
saveOutput f (n, rs) = do
  let name = takeFileName . dropExtension $ f
  o <- S.openFile f S.WriteMode
  S.hPutStrLn o name
  S.hPutStrLn o mark
  S.hPutStrLn o $ "total: " ++ show n
  S.hPutStrLn o mark
  mapM (\(w,w',n) ->  S.hPutStrLn o 
                   $  unpack w 
                   ++ "     " 
                   ++ unpack w' 
                   ++ "     "
                   ++ show n) rs
  S.hClose o
  return ()
      where mark = foldr (++) mempty $ (const "-") <$> [1..50] 


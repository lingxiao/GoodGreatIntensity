{-# LANGUAGE OverloadedStrings #-}

import Data.Word
import Data.Time
import Data.Attoparsec.Char8
import Control.Applicative
-- We import ByteString qualified because the function
-- 'Data.ByteString.readFile' would clash with
-- 'Prelude.readFile'.
import qualified Data.ByteString as B

-----------------------
------ SETTINGS -------
-----------------------

-- | File where the log is stored.
logFile :: FilePath
logFile = "sellings.log"

-----------------------
-------- TYPES --------
-----------------------

-- | Type for IP's.
data IP = IP Word8 Word8 Word8 Word8 deriving Show

data Product = Mouse | Keyboard | Monitor | Speakers deriving Show

data Source = Internet | Friend | NoAnswer deriving Show

-- show
data LogEntry =
  LogEntry { entryTime :: LocalTime
           , entryIP   :: IP
           , entryProduct   :: Product
             -- Addition of the 'Source' field
           , source    :: Source
             } deriving Show
-- /show

type Log = [LogEntry]

-----------------------
------- PARSING -------
-----------------------

-- | Parser of values of type 'IP'.
parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  d4 <- decimal
  return $ IP d1 d2 d3 d4

-- | Parser of values of type 'LocalTime'.
timeParser :: Parser LocalTime
timeParser = do
  y  <- count 4 digit
  char '-'
  mm <- count 2 digit
  char '-'
  d  <- count 2 digit
  char ' '
  h  <- count 2 digit
  char ':'
  m  <- count 2 digit
  char ':'
  s  <- count 2 digit
  return $
    LocalTime { localDay = fromGregorian (read y) (read mm) (read d)
              , localTimeOfDay = TimeOfDay (read h) (read m) (read s)
                }

-- | Parser of values of type 'Product'.
productParser :: Parser Product
productParser =
     (string "mouse"    >> return Mouse)
 <|> (string "keyboard" >> return Keyboard)
 <|> (string "monitor"  >> return Monitor)
 <|> (string "speakers" >> return Speakers)

sourceParser :: Parser Source
sourceParser =
      (string "internet" >> return Internet)
  <|> (string "friend" >> return Friend)
  <|> (string "noanswer" >> return NoAnswer)

-- show
-- | Parser of log entries.
logEntryParser :: Parser LogEntry
logEntryParser = do
  t <- timeParser
  char ' '
  ip <- parseIP
  char ' '
  p <- productParser
  -- Addition of the 'Source' field
  char ' '
  s <- sourceParser
  --
  return $ LogEntry t ip p s
-- /show

logParser :: Parser Log
logParser = many $ logEntryParser <* endOfLine

----------------------
-------- MAIN --------
----------------------

main :: IO ()
main = B.readFile logFile >>= print . parseOnly logParser

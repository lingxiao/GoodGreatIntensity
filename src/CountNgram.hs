{-# LANGUAGE ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, AllowAmbiguousTypes   #-}
-----------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module  : Counting occurences of items in data
-- | Author  : Xiao Ling
-- | Date    : 8/12/2016
-- |             
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module CountNgram where

import Prelude hiding (takeWhile, words)

--import Data.ByteString.Lazy
--import Data.Attoparsec.ByteString.Lazy

-- * mock up import, to be deleted
import GHC.Word         (Word8)


-- * tutorial imports to be deleted
import Data.Time
import Data.Word
-- import Data.Attoparsec.ByteString.Char8

import Data.Attoparsec.ByteString
import Data.ByteString.Char8
--import qualified Data.ByteString.Char8 as C


import Core

{-----------------------------------------------------------------------------
   An attoparsec tutorial
------------------------------------------------------------------------------}

data IP = IP Word8 Word8 Word8 Word8 
    deriving Show

data Product = Mouse | Keyboard | Monitor | Speakers 
    deriving (Show)

data LogEntry = LE { entryTime    :: LocalTime
                   , entryIP      :: IP 
                   , entryProduct :: Product
                   }

type Log = [LogEntry]                   

s1,s2,s3,s4,s5, s6:: ByteString
s1 = pack "hello world"
s2 = pack "world hello world"
s3 = pack "world hello hello"
s4 = pack "world world"
s5 = pack "hello\n345"
s6 = pack "hello\t120\nadele\t49292"

w1, w2 :: Word8
w1 = read "hello"
w2 = read "world"

p1 :: Parser ByteString
p1 = takeTill (\w -> w /= (read "hello" :: Word8))

-- * curent problem: bytestring is wrong datatype
-- * use text


-- * parse IP 
--parseIP :: Parser IP
--parseIP = do
--  d1 <- decimal
--  char '.'
--  d2 <- decimal
--  char '.'
--  d3 <- decimal
--  char '.'
--  d4 <- decimal
--  return $ IP d1 d2 d3 d4





{-----------------------------------------------------------------------------
   Now you need to open a file and count stuff

-- * for first iteration, don't use conduit, just regular IO
pShort, p1gm, p2gm, p5gm :: FilePath
pShort = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/short/"
p1gm    = pShort ++ "1gm.txt"
p2gm    = pShort ++ "2gm.txt"
p5gm    = pShort ++ "5gm.txt"


-- * a lazy bytestring example
bs, vocab :: ByteString
bs    = pack ([read "hello\t120\nadele\t49292" :: Word8])
vocab = pack . pure $ (read "13:07:13\t4494\n13:07:14\t4524\n13:07:15\t4480\n13:07:16\t4831\n13:07:17\t4782\n13:07:18\t4934\n13:07:19\t5133\n13:07:20\t5323\n13:07:21\t5158\n13:07:22\t4769\n13:07:23\t4744\n13:07:24\t4684\n13:07:25\t4768\n13:07:26\t4797\n13:07:27\t4947\n13:07:28\t4832\n13:07:29\t4755\n13:07:30\t5193\n13:07:31\t4774\n13:07:32\t4982\n13:07:33\t4669\n13:07:34\t4940\n13:07:35\t4894\n13:07:36\t5112\n13:07:37\t4919\n13:07:38\t4638\n13:07:39\t4557\n13:07:40\t4804\n13:07:41\t5023\n13:07:42\t5131\n13:07:43\t5286\nBelback\t222\nBelbal\t298\nBelbari\t657\nBelbas\t1456\nBelbase\t988\nBelbasi\t246\nBelbe\t1420\nBelbeck\t1205\nBelbeis\t384\nBelbek\t308\nBelbel\t976\nBelbello\t3990\nBelben\t2843\nBelbenoit\t597\nBelbeoch\t351\nBelber\t4220\nBelbes\t257\nBelbey\t298\nBelbien\t234\nBelbin\t52831\nBelbins\t754\nBelbis\t415\nBelblidia\t438\nBelbo\t4540\nBelboks\t403\nBelbora\t1278\nBelborn\t1180\nBelborough\t322\nBelbot\t507\nBelianis\t433\nBelianska\t445\nBelianske\t875\nBeliar\t5330\nBeliard\t2081\nBeliarsclew\t314\nBelias\t2632\nBeliatta\t513\nBeliau\t2791\nBeliauskene\t834\nBeliavski\t338\nBeliavsky\t8680\nBeliayeva\t326\nBelibis\t231\nBeliblog\t1307\nBelic\t9844\nBelica\t4248\nBelicanec\t268\nBelicard\t395\nBelicchi\t309\nBelice\t20191\nBelicek\t421\nBelicena\t287\nBelich\t10616\nBeliche\t599\nBelicheck\t2193\nBelichek\t1897\nBelichenko\t1672\nBelichick\t152970\nBelichickFan\t261\nBelichicks\t482\nBelichik\t1543\nBelichter\t235\nBelichtingscompensatie\t924\nBelichtung\t913\nBelichtungs\t783\nBelichtungszeit\t1090\nswimsafe\t729\nswimsauit\t363\nswimsavers\t353\nswimscuit\t256\nswimsduit\t349\nswimsear\t215\nswimseuit\t352\nswimshirts\t294\nswimshit\t382\nswimshop\t426\nswimshorts\t1279\nswimshsop\t274\nswimshuit\t340\nswimsiit\t627\nswimsit\t950\nswimsits\t461\nswimsiuit\t345\nswimsiut\t1793\nswimsiuts\t200\nswimsjit\t388\nswimsjuit\t336\nswimsocceg\t319\nswimsooit\t239\nswimspa\t701\nswimspas\t869\nswimspeed\t211\nswimssuit\t513\nswimstar27\t547\nswimstep\t560\nswimsu\t625\nswimsu7it\t349\nswimsu8it\t417\nswimsu8t\t371\nswimsu9it\t349\nswimsu9t\t382\nswimsuat\t235\nswimsueet\t274\nswimsueit\t254" :: Word8)

-- * consider the problem of how to search through bytestrings for
-- * some value
-- * note this stuff should have no conduit in it what so ever, it's completely separate from that


-- * Given vocab `w` search for number of occurences of `w` in `bs`
-- * If vocab not found, output `Nothing`
--cnt :: ByteString -> ByteString -> Maybe Int
cnt w bs = undefined

-- * we need to retrieve the line where the vocab is found
-- * make this air tight so we can swap it with attoparsec later

------------------------------------------------------------------------------}





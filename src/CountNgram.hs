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

import Prelude hiding (takeWhile)

import Control.Applicative

--import Data.Text.Lazy (Text, pack)
import Data.Char
import Data.Text hiding (takeWhile, foldr)
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator

-- * TODO: make everything here lazy
import Core

{-----------------------------------------------------------------------------
   ad hoc test
------------------------------------------------------------------------------}

s5 = pack "hello\t345\n"
s6 = pack "hello world\t345\n"
s7 = pack "hello\t120\nadele\t49292"

w1 = pack "hello"
w2 = pack "world"


p1 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/short/1gm.txt"
p2 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/short/2gm.txt"
p5 = "/Users/lingxiao/Documents/NLP/Code/Datasets/Ngrams/data/short/5gm.txt"

f1 = pack "13:07:13\t4494\n13:07:14\t4524\n13:07:15\t4480\n13:07:16\t4831\n13:07:17\t4782\n13:07:18\t4934\n13:07:19\t5133\n13:07:20\t5323\n13:07:21\t5158\n13:07:22\t4769\n13:07:23\t4744\n13:07:24\t4684\n13:07:25\t4768\n13:07:26\t4797\n13:07:27\t4947\n13:07:28\t4832\n13:07:29\t4755\n13:07:30\t5193\n13:07:31\t4774\n13:07:32\t4982\n13:07:33\t4669\n13:07:34\t4940\n13:07:35\t4894\n13:07:36\t5112\n13:07:37\t4919\n13:07:38\t4638\n13:07:39\t4557\n13:07:40\t4804\n13:07:41\t5023\n13:07:42\t5131\n13:07:43\t5286\nBelback\t222\nBelbal\t298\nBelbari\t657\nBelbas\t1456\nBelbase\t988\nBelbasi\t246\nBelbe\t1420\nBelbeck\t1205\nBelbeis\t384\nBelbek\t308\nBelbel\t976\nBelbello\t3990\nBelben\t2843\nBelbenoit\t597\nBelbeoch\t351\nBelber\t4220\nBelbes\t257\nBelbey\t298\nBelbien\t234\nBelbin\t52831\nBelbins\t754\nBelbis\t415\nBelblidia\t438\nBelbo\t4540\nBelboks\t403\nBelbora\t1278\nBelborn\t1180\nBelborough\t322\nBelbot\t507\nBelianis\t433\nBelianska\t445\nBelianske\t875\nBeliar\t5330\nBeliard\t2081\nBeliarsclew\t314\nBelias\t2632\nBeliatta\t513\nBeliau\t2791\nBeliauskene\t834\nBeliavski\t338\nBeliavsky\t8680\nBeliayeva\t326\nBelibis\t231\nBeliblog\t1307\nBelic\t9844\nBelica\t4248\nBelicanec\t268\nBelicard\t395\nBelicchi\t309\nBelice\t20191\nBelicek\t421\nBelicena\t287\nBelich\t10616\nBeliche\t599\nBelicheck\t2193\nBelichek\t1897\nBelichenko\t1672\nBelichick\t152970\nBelichickFan\t261\nBelichicks\t482\nBelichik\t1543\nBelichter\t235\nBelichtingscompensatie\t924\nBelichtung\t913\nBelichtungs\t783\nBelichtungszeit\t1090\nswimsafe\t729\nswimsauit\t363\nswimsavers\t353\nswimscuit\t256\nswimsduit\t349\nswimsear\t215\nswimseuit\t352\nswimshirts\t294\nswimshit\t382\nswimshop\t426\nswimshorts\t1279\nswimshsop\t274\nswimshuit\t340\nswimsiit\t627\nswimsit\t950\nswimsits\t461\nswimsiuit\t345\nswimsiut\t1793\nswimsiuts\t200\nswimsjit\t388\nswimsjuit\t336\nswimsocceg\t319\nswimsooit\t239\nswimspa\t701\nswimspas\t869\nswimspeed\t211\nswimssuit\t513\nswimstar27\t547\nswimstep\t560\nswimsu\t625\nswimsu7it\t349\nswimsu8it\t417\nswimsu8t\t371\nswimsu9it\t349\nswimsu9t\t382\nswimsuat\t235\nswimsueet\t274\nswimsueit\t254"
f2 = pack "measuring Time\t85\nmeasuring Tools\t53\nmeasuring Total\t285\nmeasuring True\t177\nmeasuring Type\t46\nmeasuring U\t135\nmeasuring UCE\t53\nmeasuring UDP\t97\nmeasuring UK\t61\nmeasuring US\t241\nmeasuring UV\t733\nmeasuring UVA\t69\nmeasuring UWB\t59\nmeasuring V\t540\nmeasuring VO\t71\nmeasuring VO2\t90\nmeasuring VOC\t273\nmeasuring VOCs\t215\nmeasuring VSWR\t53\nmeasuring VaR\t99\nmeasuring Value\t54\nmeasuring Vehicle\t45\nmeasuring VoIP\t136\nmeasuring W\t156\nmeasuring WOM\t43\nmeasuring WTP\t132\nmeasuring WW\t54\nmeasuring WWW\t69\nmeasuring Web\t1077\nmeasuring Wendy\t40\nmeasuring Wound\t61\nmeasuring X\t748\nmeasuring Y\t141\nmeasuring Young\t77\nmeasuring Z\t147\nmeasuring Zope\t64\nmeasuring [\t1685\nmeasuring \\\t357\nmeasuring ]\t419\nmeasuring ^\t43\nmeasuring `\t152\nmeasuring ``\t85\nmeasuring a\t106635\nmeasuring a.\t70\nmeasuring ab\t42"
f5 = pack "the specified offset before reading\t55\nthe specified offset from a\t50\nthe specified offset from the\t198\nthe specified offset in the\t758\nthe specified offset into the\t245\nthe specified offset is filled\t46\nthe specified offset is greater\t52\nthe specified offset is negative\t1023\nthe specified offset is no\t49\nthe specified offset isnegative or\t44\nthe specified offset of the\t54\nthe specified offset of this\t65\nthe specified offset to the\t266\nthe specified offset up to\t70\nthe specified offset with the\t61\nthe specified offset within the\t218\nthe specified offsets to the\t46\nthe specified old child node\t44\nthe specified ondemand runlevel is\t42\nthe specified one - dimensional\t192\nthe specified one - year\t48\nthe specified one ; </S>\t41\nthe specified one and returns\t68\nthe specified one as possible\t55\nthe specified one in the\t42\nthe specified one line record\t107\nthe specified one or more\t47\nthe specified one will be\t62\nthe specified one year period\t44\nthe specified opaque Color .\t42\nthe specified open archive file\t57\nthe specified open cursor ,\t51\nthe specified open file .\t93\nthe specified open line device\t61\nthe specified open registry key\t159\nthe specified opening times .\t184\nthe specified operand number is\t127\nthe specified operands , each\t47\nthe specified operands , so\t79\nthe specified operating conditions .\t201\nthe specified operating conditions and\t70\nthe specified operating environment it\t83\nthe specified operating mode .\t126\nthe specified operating performance under\t40\nthe specified operating pressure .\t48\nthe specified operating range ,\t45\nthe specified operating range .\t577\nthe specified operating range of\t52\nthe specified operating ranges including\t73\nthe specified operating system ,\t64\nthe specified operating system .\t96\nthe specified operating systems .\t96\nthe specified operating temperature .\t41\nthe specified operating temperature and\t48\nthe specified operating temperature range\t271\nthe specified operation , and\t56\nthe specified operation , use\t127\nthe specified operation attributes -\t45\nthe specified operation can be\t100\nthe specified operation even after\t51\nthe specified operation for policy\t233\nthe specified operation is a\t101\nthe specified operation is an\t61\nthe specified operation is denied\t63\nthe specified operation is performed\t69\nthe specified operation number .\t69\nthe specified operation on a\t41\nthe specified operation on that\t64\nthe specified operation on the\t206\nthe specified operation to the\t122\nthe specified operational limits for\t50\nthe specified operational statistics .\t42\nthe specified operations occurs on\t89\nthe specified operator , operands\t64\nthe specified operator classification of\t61\nthe specified optical file ,\t43\nthe specified option flags ,\t43\nthe specified option for the\t77\nthe specified option from the\t70\nthe specified option handler .\t44\nthe specified option in the\t213\nthe specified option is in\t139\nthe specified option of the\t47\nthe specified option to be\t71\nthe specified option to the\t52\nthe specified option with a\t42\nthe specified optional Intermediate 2\t64\nthe specified optional key as\t70\nthe specified options , and\t54\nthe specified options , just\t44\nthe specified options and class\t67\nthe specified options and properties\t42\nthe specified options in its\t57"


r5 = search f5 "the specified offset from a"
r2 = search f2 "measuring Total"
re = search f5 "hello world and stuff meh"

{-----------------------------------------------------------------------------
   Attoparsec routines specific for this application
------------------------------------------------------------------------------}

-- * search file `f` for word `w` and output its frequency
search :: Text -> String -> Maybe (String, Int)
search f w = case parse (occur w) f of
    Done _ r   -> Just r
    _          -> Nothing

-- * discard all text until word `w` occurs, and find its only field `n`
occur :: String -> Parser (String, Int)
occur w = do
    pUntil w
    string . pack $ w
    string "\t"
    n <- natural 
    string "\n"
    return (w, read n)


{-----------------------------------------------------------------------------
   Attoparsec combinators 
------------------------------------------------------------------------------}

-- * Parse a natrual number
natural :: Parser String
natural = many1' digit

-- * skip over all words in Text stream until the word we want
pUntil :: String -> Parser String 
pUntil = manyTill anyChar . lookAhead . string . pack







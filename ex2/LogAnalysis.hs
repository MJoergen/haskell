{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseList :: [String] -> LogMessage
parseList x = case x of
   ("I":t:xs)   -> LogMessage Info             (read t) (unwords xs)
   ("W":t:xs)   -> LogMessage Warning          (read t) (unwords xs)
   ("E":e:t:xs) -> LogMessage (Error (read e)) (read t) (unwords xs)
   (xs)         -> Unknown                              (unwords xs)

parseMessage :: String -> LogMessage
parseMessage x = parseList (words x)

parseMessages :: [String] -> [LogMessage]
parseMessages [] = []
parseMessages (x:xs) = parseMessage x : parseMessages xs

parse :: String -> [LogMessage]
parse x = parseMessages (lines x)
   
insert :: LogMessage -> MessageTree -> MessageTree
insert m t = case m of
   Unknown _ -> t
   _         -> t

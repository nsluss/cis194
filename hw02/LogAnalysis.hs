{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.Char (isDigit)

parseMessage :: String -> LogMessage
parseMessage message@(x:xs) = case x of
                              'I' -> parseInfo $ tail xs
                              'E' -> parseError $ tail xs
                              'W' -> parseWarning $ tail xs
                              _  -> Unknown message
  where parseInfo message' = let message'' = trimLeadingInt message'
                            in LogMessage Info (fst message'') (snd message'')
        parseError message' = let message'' = trimLeadingInt message'
                             in LogMessage (Error (fst message'')) (fst (trimLeadingInt $ snd message'')) (snd (trimLeadingInt $ snd message''))
        parseWarning message' = let message'' = trimLeadingInt message'
                               in LogMessage Warning (fst message'') (snd message'')
parseMessage [] = Unknown []

trimLeadingInt :: String -> (Int, String)
trimLeadingInt str = (trimInt str, dropInt str)
  where trimInt = read . takeWhile isDigit
        dropInt = tail . dropWhile isDigit

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert myLog@(LogMessage _ time _) tree =
  case tree of
  (Leaf) -> Node Leaf myLog Leaf
  (Node l nodeV@(LogMessage _ val _) r) -> if time > val
                                     then (Node l nodeV (insert myLog r))
                                     else (Node (insert myLog l) nodeV r)


--intentionally avoiding folds for now
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x $ build xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l val r) = inOrder l ++ [val] ++ inOrder r

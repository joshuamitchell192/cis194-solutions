{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}

module LogAnalysis2 where

import Log

parseMessage :: String -> LogMessage
parseMessage message =
  let wordList = words message
   in case wordList of
        ("I" : timestamp : messageString) -> LogMessage Info (read timestamp) (unwords messageString)
        ("W" : timestamp : messageString) -> LogMessage Warning (read timestamp) (unwords messageString)
        ("E" : lvl : timestamp : messageString) -> LogMessage (Error (read lvl)) (read timestamp) (unwords messageString)
        _ -> Unknown (unwords wordList)

parse :: String -> [LogMessage]
parse messages =
  case messages of
    x -> [parseMessage x]

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ timestamp _) (Node left treeRootMsg@(LogMessage _ rootTimestamp _) right)
  | timestamp < rootTimestamp = Node (insert msg left) treeRootMsg right
  | otherwise = Node left treeRootMsg (insert msg right)
insert _ (Node _ (Unknown _) _) =
  error "Unknown messages can be to a message tree"

insertTest :: Bool
insertTest =
  and
    [ Node Leaf info Leaf == insert info Leaf,
      Node Leaf info (Node Leaf warning Leaf) == insert warning infoTree,
      Node (Node Leaf info Leaf) warning Leaf == insert info warningTree
    ]
  where
    info = LogMessage Info 30 "doesn't matter"
    infoTree = Node Leaf info Leaf
    warning = LogMessage Warning 50 "doesn't matter"
    warningTree = Node Leaf warning Leaf

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree msg rightTree) = inOrder leftTree ++ [msg] ++ inOrder rightTree

extractMessages :: [LogMessage] -> [String]
extractMessages [] = []
extractMessages (LogMessage _ _ msg : msgs) = msg : extractMessages msgs
extractMessages _ = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = extractMessages . inOrder . build . isImportant

isImportant :: [LogMessage] -> [LogMessage]
isImportant [] = []
isImportant (msg@(LogMessage (Error lvl) _ _) : messageList)
  | lvl > 50 = msg : isImportant messageList
isImportant (_ : messageList) = isImportant messageList

-- main :: IO [LogMessage]
-- main = testWhatWentWrong parse whatWentWrong "error.log"

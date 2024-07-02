
{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- data MessageType = Info
--                 | Warning
--                 | Error Int
--                 deriving (Show, Eq)

-- type TimeStamp = Int

-- data LogMessage = LogMessage LogAnalysis.MessageType LogAnalysis.TimeStamp String
--                 | Unknown String
--                 deriving (Show, Eq)

parseMessage :: String -> LogMessage
parseMessage message = let wordList = words message in
                       case wordList of
                        ("I": timestamp : s) -> LogMessage Info (read timestamp) (unwords s)
                        ("W": timestamp : s) -> LogMessage Warning (read timestamp) (unwords s)
                        ("E": lvl : timestamp : s) -> LogMessage (Error (read lvl)) (read timestamp) (unwords s)
                        _ -> Unknown (unwords wordList)




parse :: String -> [LogMessage]
parse message = case message of
                (x) -> parseMessage x : []
                _   -> []

insert :: LogMessage -> MessageTree -> MessageTree
-- insert lmsg@LogMessage{} tree = Node tree lmsg tree
insert lmsg1@(LogMessage _ timeStamp1 _) (Node left lmsg2@(LogMessage _ timeStamp2 _) right)
    | timeStamp1 > timeStamp2 = Node left lmsg2 (insert lmsg1 right)
    | otherwise = Node (insert lmsg1 left) lmsg2 right
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = extractMessage . inOrder . build . filter (isSevere 50)

isSevere :: Int -> LogMessage -> Bool
isSevere minLvl (LogMessage (Error lvl) _ _)
    | lvl > minLvl = True
    | otherwise = False
isSevere _ _ = False

extractMessage :: [LogMessage] -> [String]
extractMessage (LogMessage _ _ msg : msgs) = msg : extractMessage msgs
extractMessage _ = []

-- result :: IO [String]
print testWhatWentWrong parse whatWentWrong "./sample.log"

                    
-- result :: [LogMessage]

-- printLogMessages


main :: IO ()
main = return ()
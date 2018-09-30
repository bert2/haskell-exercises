{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where 
    
import Log

parseMessage :: String -> LogMessage
parseMessage msg =
    case words msg of
        "I":t:ws   -> LogMessage Info (read t) (unwords ws)
        "W":t:ws   -> LogMessage Warning (read t) (unwords ws)
        "E":s:t:ws -> LogMessage (Error $ read s) (read t) (unwords ws)
        _          -> Unknown msg

parse :: String -> [LogMessage]
parse = map parseMessage . lines

getMessage :: LogMessage -> String
getMessage (LogMessage _ t msg) = show t ++ ": " ++ msg
getMessage (Unknown msg)        = msg

insert :: LogMessage -> MessageTree -> MessageTree
insert _ (Node _ (Unknown _) _) = error "invalid log tree"
insert (Unknown _) ms = ms
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ t _) (Node ls m'@(LogMessage _ t' _) rs)
    | t < t'    = Node (insert m ls) m' rs
    | otherwise = Node ls m' (insert m rs)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node ls m rs) = inOrder ls ++ [m] ++ inOrder rs

isInfo :: LogMessage -> Bool
isInfo (LogMessage Info _ _) = True
isInfo _                     = False

isWarning :: LogMessage -> Bool
isWarning (LogMessage Warning _ _) = True
isWarning _                        = False

isError :: Int -> LogMessage -> Bool
isError s (LogMessage (Error s') _ _) = s' >= s
isError _ _                           = False

isUnknown :: LogMessage -> Bool
isUnknown (Unknown _) = True
isUnknown _           = False

between :: Int -> Int -> LogMessage -> Bool
between mint maxt (LogMessage _ t _) = mint <= t && t <= maxt
between _ _ _                        = False

whatHappend :: (LogMessage -> Bool) -> [LogMessage] -> [String]
whatHappend f = map getMessage . inOrder . build . filter f

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = whatHappend (isError 50)

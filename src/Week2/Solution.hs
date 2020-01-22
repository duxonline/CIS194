module Week2.Solution where
data MessageType = Info
                | Warning
                | Error Int
                deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
                deriving (Show, Eq)

data MessageTree = Leaf
                | Node MessageTree LogMessage MessageTree                

parseMessage :: String -> LogMessage
parseMessage s = 
    case words s of
        "I":ts:m -> LogMessage Info (read ts) (unwords m)
        "W":ts:m -> LogMessage Warning (read ts) (unwords m)
        "E":level:ts:m -> LogMessage (Error (read level)) (read ts) (unwords m)
        _ -> Unknown s

parseLog :: String -> [LogMessage]
parseLog log = map parseMessage $ lines log

parseLogFile :: FilePath -> IO [LogMessage]
parseLogFile path = parseLog <$> readFile path 

insert :: LogMessage -> MessageTree -> MessageTree
insert msg@(Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ m1 _) tree@(Node left msg2@(LogMessage _ m2 _) right) 
    | m1 < m2 = Node (insert msg1 left) msg2 right
    | otherwise = Node left msg2 (insert msg1 right)

build :: [LogMessage] -> MessageTree
-- build [] = Leaf
-- build (x:zs) = insert x $ build zs
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right
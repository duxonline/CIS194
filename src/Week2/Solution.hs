module Week2.Solution where
data MessageType = Info
                | Warning
                | Error Int
                deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
                deriving (Show, Eq)

parseMessage :: String -> LogMessage
parseMessage s = 
    case words s of
        "I":ts:m -> LogMessage Info (read ts) (unwords m)
        "W":ts:m -> LogMessage Warning (read ts) (unwords m)
        "E":level:ts:m -> LogMessage (Error (read level)) (read ts) (unwords m)
        _ -> Unknown s

parse :: String -> [LogMessage]
parse log = map parseMessage $ lines log

readLogFile :: FilePath -> IO [LogMessage]
readLogFile path = parse <$> readFile path 
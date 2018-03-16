{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

maybeTimestamp :: String -> Maybe TimeStamp
maybeTimestamp ts = case reads ts of
    [(x, "")] -> Just x
    _   -> Nothing

parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
    ("I":t:rest)         -> LogMessage Info (getTime t) (unwords rest)
    ("W":t:rest)         -> LogMessage Warning (getTime t) (unwords rest)
    ("E":errcode:t:rest) -> LogMessage (Error $ read errcode) (getTime t) (unwords rest)
    rest                 -> Unknown (unwords rest)
    where
        getTime :: String -> TimeStamp
        getTime ts = case maybeTimestamp ts of
            Just a  -> a
            Nothing -> 0

parse :: String -> [LogMessage]
parse msgs = map parseMessage (lines msgs)

insert :: LogMessage -> MessageTree -> MessageTree
insert msg         Leaf = Node Leaf msg Leaf
insert (Unknown _) tree = tree
insert msg@(LogMessage _ newts _) tree@(Node l tMsg@(LogMessage _ ts _) r)
    | newts == ts = tree
    | newts <  ts = Node (insert msg l) tMsg r
    | newts >  ts = Node l tMsg (insert msg r)
insert _ _              = Leaf

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getErrorMessage . inOrder . build . filter isMajorError
  where isMajorError (LogMessage (Error s) _ _) = s >= 50
        isMajorError _ = False
        getErrorMessage (LogMessage _ _ msg) = msg
        getErrorMessage _ = "This case shall never happen"

module Utils where

readWhile :: (Char -> Bool) -> Handle -> IO String
readWhile p handle = do
    char <- hLookAhead handle
    if p char
        then do
            _ <- hGetChar handle
            rest <- readWhile p handle
            return (char : rest)
        else return []
        
isScalarChar :: Char -> Bool
isScalarChar c = not (isSpace c || c `elem` [':', '-', '"', '\n', '\r', '\t'])
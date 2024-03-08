module YAMLTokenizer where
import System.IO
import Data.Char (isSpace)
import Utils
import qualified Data.Token as Token
spacesCount :: Handle -> IO Int
spacesCount handle = do
    spaces <- readWhile (== ' ') handle  -- Read consecutive spaces
    return (1 + length spaces)
tokenize handle = do
    isEOF <- hIsEOF handle
    if not isEOF
        then do
            char <- hGetChar handle
            case char of
                ':' -> (Token.Colon :) <$> tokenize handle
                '-' -> (Token.Dash :) <$> tokenize handle
                '\n' -> (Token.NewLine :) <$> tokenize handle
                '{' -> (Token.MappingStart :) <$> tokenize handle
                '}' -> (Token.MappingEnd :) <$> tokenize handle
                '[' -> (Token.SequenceStart :) <$> tokenize handle
                ']' -> (Token.SequenceEnd :) <$> tokenize handle
                ',' -> (Token.Comma :) <$> tokenize handle
                ' ' -> do
                    numSpaces <- spacesCount handle
                    (Token.Space numSpaces :) <$> tokenize handle
                '#' -> do
                    str <- readWhile (/= '\n') handle
                    (Token.Comment str :) <$> tokenize handle
                '\'' -> do
                    str <- readUntilQuote handle
                    (Token.Scalar str 2 :) <$> tokenize handle
                '"' -> do
                    str <- readUntilQuote handle
                    (Token.Scalar str 1 :) <$> tokenize handle
                _ -> do
                    let str = [char]
                    rest <- readWhile isScalarChar handle
                    let fullStr = str ++ rest
                    (Token.Scalar fullStr 0 :) <$> tokenize handle
        else
            return [Token.EOF]

readUntilQuote :: Handle -> IO String
readUntilQuote handle = do
    char <- hGetChar handle
    if char == '"'
        then return ""
        else do
            rest <- readWhile (/= '"') handle
            let str = char : rest
            nextChar <- hGetChar handle
            if nextChar == '"'
                then return str
                else (str ++) <$> readUntilQuote handle
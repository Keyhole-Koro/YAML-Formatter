module YAMLTokenizer where

import System.IO
import Data.Char (isSpace)

import qualified Data.Token as Tk

import Utils

spacesCount :: Handle -> IO Int
spacesCount handle = do
    spaces <- readWhile (== ' ') handle  -- Read consecutive spaces
    return (1 + length spaces)


tokenize :: Handle -> IO [Tk.Token]
tokenize handle = 
    tokenize' handle 1 1

tokenize' :: Handle -> Tk.LineNumber -> Tk.TokenIndex -> IO [Tk.Token]
tokenize' handle lineNum tokenIdx = do
    isEOF <- hIsEOF handle
    if not isEOF
        then do
            char <- hGetChar handle
            case char of
                ':' -> (Tk.TokenRec Tk.Colon lineNum tokenIdx :) <$> tokenize' handle lineNum (tokenIdx + 1)
                '-' -> (Tk.TokenRec Tk.Dash lineNum tokenIdx :) <$> tokenize' handle lineNum (tokenIdx + 1)
                '\n' -> (Tk.TokenRec Tk.NewLine lineNum tokenIdx :) <$> tokenize' handle (lineNum + 1) 0
                '{' -> (Tk.TokenRec Tk.MappingStart lineNum tokenIdx :) <$> tokenize' handle lineNum (tokenIdx + 1)
                '}' -> (Tk.TokenRec Tk.MappingEnd lineNum tokenIdx :) <$> tokenize' handle lineNum (tokenIdx + 1)
                '[' -> (Tk.TokenRec Tk.SequenceStart lineNum tokenIdx :) <$> tokenize' handle lineNum (tokenIdx + 1)
                ']' -> (Tk.TokenRec Tk.SequenceEnd lineNum tokenIdx :) <$> tokenize' handle lineNum (tokenIdx + 1)
                ',' -> (Tk.TokenRec Tk.Comma lineNum tokenIdx :) <$> tokenize' handle lineNum (tokenIdx + 1)
                ' ' -> do
                    numSpaces <- spacesCount handle
                    (Tk.TokenRec (Tk.Space numSpaces) lineNum tokenIdx :) <$> tokenize' handle lineNum (tokenIdx + 1)
                '#' -> do
                    str <- readWhile (/= '\n') handle
                    (Tk.TokenRec (Tk.Comment str) lineNum tokenIdx :) <$> tokenize' handle lineNum (tokenIdx + 1)
                '\'' -> do
                    str <- readUntilQuote handle
                    (Tk.TokenRec (Tk.Scalar str 2) lineNum tokenIdx :) <$> tokenize' handle lineNum (tokenIdx + 1)
                '"' -> do
                    str <- readUntilQuote handle
                    (Tk.TokenRec (Tk.Scalar str 1) lineNum tokenIdx :) <$> tokenize' handle lineNum (tokenIdx + 1)
                _ -> do
                    let str = [char]
                    rest <- readWhile isScalarChar handle
                    let fullStr = str ++ rest
                    (Tk.TokenRec (Tk.Scalar fullStr 0) lineNum tokenIdx :) <$> tokenize' handle lineNum (tokenIdx + 1)
        else
            return [Tk.TokenRec Tk.EOF lineNum tokenIdx ]

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

-- You may need to define isScalarChar, readWhile, and other utility functions used in your code.

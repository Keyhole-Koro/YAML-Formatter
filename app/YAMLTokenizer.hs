module YAMLTokenizer where

import System.IO
import Data.Char (isSpace)

import qualified Data.Token as Tk

import Error.ErrorDummy (dummyErr)
import Utils

spacesCount :: Handle -> IO Int
spacesCount handle = do
    spaces <- readWhile (== ' ') handle  -- Read consecutive spaces
    return (1 + length spaces)


tokenize :: Handle -> IO [Tk.Token]
tokenize handle = do
    isEOF <- hIsEOF handle
    if not isEOF
        then do
            char <- hGetChar handle
            case char of
                ':' -> (Tk.TokenRec Tk.Colon dummyErr :) <$> tokenize handle
                '-' -> (Tk.TokenRec Tk.Dash dummyErr :) <$> tokenize handle
                '\n' -> (Tk.TokenRec Tk.NewLine dummyErr :) <$> tokenize handle
                '{' -> (Tk.TokenRec Tk.MappingStart dummyErr :) <$> tokenize handle
                '}' -> (Tk.TokenRec Tk.MappingEnd dummyErr :) <$> tokenize handle
                '[' -> (Tk.TokenRec Tk.SequenceStart dummyErr :) <$> tokenize handle
                ']' -> (Tk.TokenRec Tk.SequenceEnd dummyErr :) <$> tokenize handle
                ',' -> (Tk.TokenRec Tk.Comma dummyErr :) <$> tokenize handle
                ' ' -> do
                    numSpaces <- spacesCount handle
                    (Tk.TokenRec (Tk.Space numSpaces) dummyErr :) <$> tokenize handle
                '#' -> do
                    str <- readWhile (/= '\n') handle
                    (Tk.TokenRec (Tk.Comment str) dummyErr :) <$> tokenize handle
                '\'' -> do
                    str <- readUntilQuote handle
                    (Tk.TokenRec (Tk.Scalar str 2) dummyErr :) <$> tokenize handle
                '"' -> do
                    str <- readUntilQuote handle
                    (Tk.TokenRec (Tk.Scalar str 1) dummyErr :) <$> tokenize handle
                _ -> do
                    let str = [char]
                    rest <- readWhile isScalarChar handle
                    let fullStr = str ++ rest
                    (Tk.TokenRec (Tk.Scalar fullStr 0) dummyErr :) <$> tokenize handle
        else
            return [Tk.TokenRec Tk.EOF dummyErr]

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

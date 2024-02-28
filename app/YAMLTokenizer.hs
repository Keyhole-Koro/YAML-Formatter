module YAMLTokenizer where

import System.IO
import Data.Char (isSpace)

import Utils

import qualified Data.Token as Token

tokenize :: Handle -> IO [Token.Token]
tokenize handle = do
    isEOF <- hIsEOF handle
    if not isEOF
        then do
            char <- hGetChar handle
            case char of
                ':' -> (Token.Colon :) <$> tokenize handle
                '-' -> (Token.Dash :) <$> tokenize handle
                '\n' -> (Token.NewLine :) <$> tokenize handle
                ' ' -> (Token.Space :) <$> tokenize handle
                '#' -> do
                    str <- readWhile (/= '\n') handle
                    let tokenList = [Token.Sharp, Token.Scalar str]
                    restTokens <- tokenize handle
                    return (tokenList ++ restTokens)
                '"' -> do
                    str <- readUntilQuote handle
                    (Token.Scalar str :) <$> tokenize handle
                _ -> do
                    let str = [char]
                    rest <- readWhile isScalarChar handle
                    let fullStr = str ++ rest
                    (Token.Scalar fullStr :) <$> tokenize handle
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

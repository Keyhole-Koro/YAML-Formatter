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
                '-' -> do
                    nextChar <- hGetChar handle
                    case nextChar of
                        '-' -> (Token.Comment :) <$> tokenize handle
                        _ -> (Token.Dash :) <$> tokenize handle
                '\n' -> (Token.NewLine :) <$> tokenize handle
                ' ' -> do
                    numSpaces <- spacesCount handle
                    (Token.Space numSpaces :) <$> tokenize handle
                '#' -> do
                    str <- readWhile (/= '\n') handle
                    let tokenList = [Token.Sharp, Token.Scalar str]
                    restTokens <- tokenize handle
                    return (tokenList ++ restTokens)
                '"' -> do
                    str <- readUntilQuote handle
                    (Token.QuotedScalar str :) <$> tokenize handle
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

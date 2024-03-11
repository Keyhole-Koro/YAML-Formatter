module YAMLTokenizer where
import System.IO

import Data.Char (isSpace)
import Utils
import qualified Data.Token as Tk
import qualified Data.ScalarTypes as St

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
                ':' -> (Tk.Colon :) <$> tokenize handle
                '-' -> (Tk.Dash :) <$> tokenize handle
                '\n' -> (Tk.NewLine :) <$> tokenize handle
                '{' -> (Tk.MappingStart :) <$> tokenize handle
                '}' -> (Tk.MappingEnd :) <$> tokenize handle
                '[' -> (Tk.SequenceStart :) <$> tokenize handle
                ']' -> (Tk.SequenceEnd :) <$> tokenize handle
                ',' -> (Tk.Comma :) <$> tokenize handle
                ' ' -> do
                    numSpaces <- spacesCount handle
                    (Tk.Space numSpaces :) <$> tokenize handle
                '#' -> do
                    str <- readWhile (/= '\n') handle
                    (Tk.Comment str :) <$> tokenize handle
                '\'' -> do
                    str <- readUntilQuote handle
                    (Tk.Scalar str St.DoubleQuote :) <$> tokenize handle
                '"' -> do
                    str <- readUntilQuote handle
                    (Tk.Scalar str St.Quote :) <$> tokenize handle
                '|' -> do
                    nextChar <- hLookAhead handle
                    case nextChar of
                        '+' -> do
                            _ <- hGetChar handle -- consume
                            (Tk.LiteralBlockPlusStart :) <$> tokenize handle
                        '-' -> do
                            _ <- hGetChar handle
                            (Tk.LiteralBlockSMinustart :) <$> tokenize handle
                        '\n' -> (Tk.LiteralBlockStart :) <$> tokenize handle
                        _ -> tokenize handle
                '>' -> do
                    nextChar <- hLookAhead handle
                    case nextChar of
                        '+' -> do
                            _ <- hGetChar handle
                            (Tk.FoldedBlockPlusStart :) <$> tokenize handle
                        '-' -> do
                            _ <- hGetChar handle
                            (Tk.FoldedBlockMinusStart :) <$> tokenize handle
                        '\n' -> (Tk.FoldedBlockStart :) <$> tokenize handle
                        _ -> tokenize handle
                _ -> do
                    let str = [char]
                    rest <- readWhile isScalarChar handle
                    let fullStr = str ++ rest
                    (Tk.Scalar fullStr St.NoQuote :) <$> tokenize handle
        else
            return [Tk.EOF]

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

tokenizeBlock :: [Tk.Tk] -> [Tk.Tk]
tokenizeBlock (tkn:rest) =
    case tkn of
        Tk.LiteralBlockStart -> 
        Tk.FoldedBlockStart -> 
        _ -> tkn : tokenizeBlock rest

readTknUntilBlockEnds :: [Tk.Tk] -> [Tk.Tk]
readTknUntilBlockEnds (tkn:rest) =
    case tkn of
        Tk.NewLine -> readTknUntilBlockEnds' rest
    where
        readTknUntilBlockEnds' [Tk.Tk] -> [Tk.Tk]
        readTknUntilBlockEnds' (tkn:rest) =
            case tkn of
                Tk.Space n -> 
                _ -> 
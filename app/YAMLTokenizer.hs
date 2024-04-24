module YAMLtokenizer where
import System.IO
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Data.Char (isSpace)
import Utils
import Data.Token (Token(..), Kind(..))
import Data.ScalarTypes (ST(..), BS(..))

spacesCount :: Handle -> IO Int
spacesCount handle = do
    spaces <- readWhile (== ' ') handle  -- Read consecutive spaces
    return (1 + length spaces)

createToken :: Kind -> IO Token
createToken kind lineRef colRef = do
    line <- readIORef lineRef
    col <- readIORef colRef
    return $ Token kind line col

lineRef :: IORef Int
lineRef = unsafePerformIO (newIORef 0)

colRef :: IORef Int
colRef = unsafePerformIO (newIORef 0)

tokenize :: Handle -> IO [Token]
tokenize handle = do
    isEOF <- hIsEOF handle
    if not isEOF
        then do
            char <- hGetChar 
            modifyIORef' colRef (+1)
            case char of
                ':' -> (createToken Kind.Colon :) <$> tokenize handle
                '-' -> (createToken Kind.Dash :) <$> tokenize handle
                '\n' -> do
                    modifyIORef lineRef (+1)
                    writeIORef colRef (0)
                    (createToken Kind.NewLine :) <$> tokenize handle
                '{' -> (createToken Kind.MappingStart :) <$> tokenize handle
                '}' -> (createToken Kind.MappingEnd :) <$> tokenize handle
                '[' -> (createToken Kind.SequenceStart :) <$> tokenize handle
                ']' -> (createToken Kind.SequenceEnd :) <$> tokenize handle
                ',' -> (createToken Kind.Comma :) <$> tokenize handle
                ' ' -> do
                    numSpaces <- spacesCount handle
                    modifyIORef colRef (+ numSpaces)
                    (createToken (Kind.Space numSpaces) :) <$> tokenize handle
                '#' -> do
                    str <- readWhile (/= '\n') handle
                    modifyIORef colRef (+ length str)
                    (createToken (Kind.Comment str) :) <$> tokenize handle
                '\'' -> do
                    str <- readUntilQuote handle
                    modifyIORef colRef (+ length str)
                    (createToken (Kind.Scalar str ST.DoubleQuote) :) <$> tokenize handle
                '"' -> do
                    str <- readUntilQuote handle
                    modifyIORef colRef (+ length str)
                    (createToken (Kind.Scalar str ST.Quote) :) <$> tokenize handle
                '|' -> do
                    nextChar <- hLookAhead handle
                    let bs = case nextChar of
                        '+' -> BS.Plus
                        '-' -> BS.Minus
                        '\n' -> BS.Empty
                        _ -> BS.Empty
                    if bs /= BS.Empty
                        then do
                            _ <- hGetChar handle
                    (createToken (Kind.LiteralBlockStart "" bs) :) <$> tokenize handle
                '>' -> do
                    nextChar <- hLookAhead handle
                    let bs = case nextChar of
                        '+' -> BS.Plus
                        '-' -> BS.Minus
                        '\n' -> BS.Empty
                        _ -> BS.Empty
                    if bs /= BS.Empty
                        then do
                            _ <- hGetChar handle
                    (createToken (Kind.FoldedBlockStart "" bs) :) <$> tokenize handle
                _ -> do
                    let str = [char]
                    rest <- readWhile isScalarChar handle
                    let fullStr = str ++ rest
                    (createToken (Kind.Scalar fullStr ST.NoQuote) :) <$> tokenize handle
        else
            return [createToken Kind.EOF]


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

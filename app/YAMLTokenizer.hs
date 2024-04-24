module YAMLtokenize'r where
import System.IO
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Data.Char (isSpace)
import Utils
import Data.Token (Token(..), Kind(..))
import Data.ScalarTypes (ST(..), BS(..))

import Error.ErrorKind (ErrKind)

spacesCount :: Handle -> IO Int
spacesCount handle = do
    spaces <- readWhile (== ' ') handle
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
tokenize handle =
    refactorToken <$> tokenize' handle

tokenize' :: Handle -> IO [Token]
tokenize' handle = do
    isEOF <- hIsEOF handle
    if not isEOF
        then do
            char <- hGetChar 
            modifyIORef colRef (+1)
            case char of
                ':' -> (createToken Kind.Colon :) <$> tokenize' handle
                '-' -> (createToken Kind.Dash :) <$> tokenize' handle
                '\n' -> do
                    modifyIORef lineRef (+1)
                    writeIORef colRef (0)
                    (createToken Kind.NewLine :) <$> tokenize' handle
                '{' -> (createToken Kind.MappingStart :) <$> tokenize' handle
                '}' -> (createToken Kind.MappingEnd :) <$> tokenize' handle
                '[' -> (createToken Kind.SequenceStart :) <$> tokenize' handle
                ']' -> (createToken Kind.SequenceEnd :) <$> tokenize' handle
                ',' -> (createToken Kind.Comma :) <$> tokenize' handle
                ' ' -> do
                    numSpaces <- spacesCount handle
                    modifyIORef colRef (+ numSpaces)
                    (createToken (Kind.Space numSpaces) :) <$> tokenize' handle
                '#' -> do
                    str <- readWhile (/= '\n') handle
                    modifyIORef colRef (+ length str)
                    (createToken (Kind.Comment str) :) <$> tokenize' handle
                '\'' -> do
                    line <- readWhile (/='\n')
                    let lastChar = case getLastChar line of
                                Just c -> c
                                Nothing -> error "Empty line"
                    let (str, kind) = if lastChar == '\''
                            then (readUntilQuote handle, Kind.SingleQuote)
                            else if lastChar == '\''
                                    then (line, Kind.Scalar)
                                    else ("", ErrKind.MissingQuote)
                    modifyIORef colRef (+ length str)
                    let op_kind = if kind == Kind.scalar
                        then Kind.Scalar str ST.Quote
                        else kind
                    (createToken op_kind :) <$> tokenize' handle
                '"' -> do
                    line <- readWhile (/='\n')
                    let lastChar = case getLastChar line of
                                Just c -> c
                                Nothing -> error "Empty line"
                    let (str, kind) = if lastChar == '"'
                            then (readUntilDoubleQuote handle, Kind.Scalar)
                            else if lastChar == '"'
                                    then (line, Kind.DoubleQuote)
                                    else ("", ErrKind.MissingDoubleQuote)
                    modifyIORef colRef (+ length str)
                    let op_kind = if kind == Kind.scalar
                        then Kind.Scalar str ST.DoubleQuote
                        else kind
                    (createToken op_kind :) <$> tokenize' handle
                '|' -> do
                    nextChar <- hLookAhead handle
                    let bs = case nextChar of
                        '+' -> BS.Plus
                        '-' -> BS.Minus
                        '\n' -> BS.Empty
                        _ -> BS.Empty
                    if bs /= BS.Empty
                        then _ <- hGetChar handle
                    (createToken (Kind.LiteralBlockStart "" bs) :) <$> tokenize' handle
                '>' -> do
                    nextChar <- hLookAhead handle
                    let bs = case nextChar of
                        '+' -> BS.Plus
                        '-' -> BS.Minus
                        '\n' -> BS.Empty
                        _ -> BS.Empty
                    if bs /= BS.Empty
                        then _ <- hGetChar handle
                    (createToken (Kind.FoldedBlockStart "" bs) :) <$> tokenize' handle
                _ -> do
                    let str = [char]
                    rest <- readWhile isScalarChar handle
                    let fullStr = str ++ rest
                    modifyIORef colRef (+ length fullStr)
                    (createToken (Kind.Scalar fullStr ST.NoQuote) :) <$> tokenize' handle
        else
            return [createToken Kind.EOF]

refactorToken :: [Token] -> [Token]
refactorToken [] = []
refactorToken tokens@(tkn:rest) =
    case kind tkn of
        Kind.LiteralBlockStart _ _ -> constructBlockScalarToken tokens (column tkn)
        Kind.FoldedBlockStart _ _ -> constructBlockScalarToken tokens (column tkn)
        _ -> tkn : refactorToken rest

constructBlockScalarToken :: [Token] -> Int -> [Token]
constructBlockScalarToken [] _ = []
constructBlockScalarToken tokens@(tkn:rest) expectedPos =
    let (str, remainingTokens) = readUntilBlockScalarEnds tokens expectedPos
    in case tkn of
        Token (Kind.LiteralBlockStart _ bs) line column -> Token (Kind.LiteralBlockStart str bs) line column : remainingTokens
        Token (Kind.FoldedBlockStart _ bs) line column -> Token (Kind.FoldedBlockStart str bs) line column : remainingTokens
        _ -> remainingTokens

readUntilBlockScalarEnds :: [Token] -> Int -> (String, [Token])
readUntilBlockScalarEnds [] _ = ("", [])
readUntilBlockScalarEnds tokens expectedPos = readUntilBlockScalarEnds' tokens expectedPos ""

readUntilBlockScalarEnds' :: [Token] -> Int -> String -> (String, [Token])
readUntilBlockScalarEnds' [] _ fullStr = (fullStr, [])
readUntilBlockScalarEnds' tokens@(tkn:rest) expectedPos fullStr =
    case kind (tkn) of
        Kind.NewLine -> readUntilBlockScalarEnds' rest expectedPos (fullStr ++ "\n")
        Kind.Space n -> if n >= expectedPos
                            then readUntilBlockScalarEnds' rest expectedPos (fullStr ++ spaceStr (n - expectedPos))
                            else (fullStr, tokens)
        Kind.Scalar str type' -> let str' = case type' of
                                                ST.Quote -> '\'' : str ++ '\''
                                                ST.DoubleQuote -> '"' : str ++ '"'
                                                ST.NoQuote -> str
                                 in readUntilBlockScalarEnds' rest expectedPos (fullStr ++ str')
        _ -> (fullStr, tokens)


readUntilQuote :: Handle -> IO String
readUntilQuote handle = do
    char <- hGetChar handle
    if char == '\''
        then return ""
        else do
            rest <- readWhile (/= '\'') handle
            let str = char : rest
            nextChar <- hGetChar handle
            if nextChar == '\''
                then return str
                else (str ++) <$> readUntilQuote handle

readUntilDoubleQuote :: Handle -> IO String
readUntilDoubleQuote handle = do
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

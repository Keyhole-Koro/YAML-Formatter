module YAMLValidateSyntax where

import YAMLTokenizer
import Utils

import qualified Data.Token as Token
import qualified Data.Error as Error

validateYAMLSyntax :: [Token.Token] -> IO ()
validateYAMLSyntax tokens = do
    result <- validateYAMLSyntaxOrder' tokens
    case result of
        Just errorPos -> putStrLn $ "Syntax error at position " ++ show errorPos
        Nothing -> putStrLn "YAML syntax is correct"

processNewlineSpaces :: [Token] -> Int -> Int -> [Token] -> [Error] -> (Int, [Error])
processNewlineSpaces tokens@(tkn:rest) tknIndex reserveTkn errors =
    let spcTkn = readWhile (== Token.Space) tokens
        spacesCount = length spcTkn
        errorMsg
            | spacesCount > 2 = "Excessive number of spaces"
            | spacesCount < 2 = "Insufficient number of spaces"
            | otherwise = "Correct number of spaces"
        newError = if errorMsg /= "Correct number of spaces"
                      then ([ErrorKind.IndentSpaces
                            tknIndex errorMsg
                            ("There is(are) " ++ show spacesCount ++ " space(s)") |
                            errorMsg /= "Correct number of spaces"])
                      else []
    in (spacesCount, errors ++ newError)

validateSpaceRecursive :: [Token.Token] -> Int -> [Token.Token] -> [Error] -> [Error]
validateSpaceRecursive [] _ _ errors = errors
validateSpaceRecursive (tkn:rest) tknIndex reserveTkn errors =
    case tkn of
        Token.NewLine ->
            let (spacesCount, newError) = processNewlineSpaces rest tknIndex reserveTkn errors
            in validateSpaceRecursive (drop spacesCount rest) (tknIndex + spacesCount) reserveTkn newError
        Token.Space -> 
            case head reserveTkn of
                Token.value ->
                    let () = 
        _ -> 
            let newError = [ErrorKind.ExcessSpaces tknIndex "Excessive number of spaces" undefined]
            in validateSpaceRecursive rest (tknIndex + 1) reserveTkn (errors ++ newError)


identifyScalar :: [Token.Token] -> IO [Token.Token]
identifyScalar [] = return []
identifyScalar (tkn:rest) =
    case tkn of
        Token.Scalar str ->
            case rest of
                (Token.Colon:xs) -> do
                    nextTokens <- identifyScalar xs
                    return (Token.Key str: Token.Colon: nextTokens)
                _ -> do
                    nextTokens <- identifyScalar rest
                    return (tkn : nextTokens)
        Token.Dash ->
            case rest of
                (Token.Space n: Token.Scalar str:xs) -> do
                    nextTokens <- identifyScalar xs
                    return (Token.Dash: Token.Space n: Token.Item str: nextTokens)
                _ -> do
                    nextTokens <- identifyScalar rest
                    return (tkn : nextTokens)
        Token.Sharp ->
            case rest of
                (Token.Space n: Token.Scalar )

        _ -> do
            nextTokens <- identifyScalar rest
            return (tkn : nextTokens)


validateYAMLSyntaxOrder' :: [Token.Token] -> IO (Maybe Int)
validateYAMLSyntaxOrder' [] = return Nothing
validateYAMLSyntaxOrder' (Token.Sharp:Token.Scalar msg:Token.NewLine:xs) = do
    putStrLn $ "Warning: Sharp followed by Scalar \"" ++ msg ++ "\""
    validateYAMLSyntaxOrder' xs
validateYAMLSyntaxOrder' (Token.Sharp:xs) = return $ Just 0 -- Error: Token.Sharp without Token.Scalar
validateYAMLSyntaxOrder' (Token.Scalar _:Token.Colon:Token.Space:xs) = validateYAMLSyntaxOrder' xs
validateYAMLSyntaxOrder' (Token.Scalar _:Token.Space:Token.Scalar _:Token.Space:Token.Sharp:Token.Scalar msg:Token.NewLine:xs) = do
    putStrLn $ "Syntax error: " ++ msg
    validateYAMLSyntaxOrder' xs
validateYAMLSyntaxOrder' (Token.Scalar _:Token.Space:Token.Scalar _:Token.NewLine:xs) = do
    putStrLn "Error: Missing Colon after key"
    validateYAMLSyntaxOrder' xs
validateYAMLSyntaxOrder' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:Token.Dash:Token.Space:_:xs) = validateYAMLSyntaxOrder' xs
validateYAMLSyntaxOrder' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:Token.Dash:Token.Space:Token.Scalar _:xs) = validateYAMLSyntaxOrder' xs
validateYAMLSyntaxOrder' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:Token.Dash:xs) = return $ Just 0 -- Error: Token.Scalar missing after Token.Dash
validateYAMLSyntaxOrder' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:_:xs) = do
    putStrLn "Error: Unexpected token after Dash"
    validateYAMLSyntaxOrder' xs
validateYAMLSyntaxOrder' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:Token.Space:Token.Scalar _:Token.Colon:Token.Space:_:xs) = validateYAMLSyntaxOrder' xs
validateYAMLSyntaxOrder' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:Token.Space:Token.Scalar _:Token.Colon:Token.Space:Token.Space:Token.Sharp:Token.Scalar msg:Token.NewLine:xs) = do
    putStrLn $ "Syntax error: " ++ msg
    validateYAMLSyntaxOrder' xs
validateYAMLSyntaxOrder' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:Token.Space:Token.Scalar _:Token.Colon:Token.Space:Token.Space:xs) = do
    putStrLn "Error: Missing Colon after key"
    validateYAMLSyntaxOrder' xs
validateYAMLSyntaxOrder' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:Token.Space:Token.Scalar _:Token.Colon:Token.Space:xs) = do
    putStrLn "Error: Missing value after Colon"
    validateYAMLSyntaxOrder' xs
validateYAMLSyntaxOrder' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:Token.Space:Token.Scalar _:xs) = do
    putStrLn "Error: Missing Colon after key"
    validateYAMLSyntaxOrder' xs
validateYAMLSyntaxOrder' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:xs) = do
    putStrLn "Error: Missing value after Colon"
    validateYAMLSyntaxOrder' xs
validateYAMLSyntaxOrder' (Token.Scalar _:Token.Colon:Token.NewLine:xs) = return $ Just 0 -- Error: Token.Scalar missing after Token.Colon
validateYAMLSyntaxOrder' (Token.Scalar _:Token.NewLine:xs) = do
    putStrLn "Error: Missing Colon after key"
    validateYAMLSyntaxOrder' xs
validateYAMLSyntaxOrder' (Token.Colon:Token.Space:Token.Scalar _:xs) = validateYAMLSyntaxOrder' xs
validateYAMLSyntaxOrder' (Token.Colon:xs) = return $ Just 0 -- Error: Token.Colon without key
validateYAMLSyntaxOrder' (Token.Space:xs) = validateYAMLSyntaxOrder' xs
validateYAMLSyntaxOrder' (Token.Dash:Token.Space:Token.Scalar _:xs) = validateYAMLSyntaxOrder' xs
validateYAMLSyntaxOrder' (Token.Dash:xs) = return $ Just 0 -- Error: Token.Dash without Token.Scalar
validateYAMLSyntaxOrder' (Token.NewLine:xs) = validateYAMLSyntaxOrder' xs
validateYAMLSyntaxOrder' (Token.EOF:_) = return Nothing
validateYAMLSyntaxOrder' _ = return $ Just 0 -- Error: Unexpected token

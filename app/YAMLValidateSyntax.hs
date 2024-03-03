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

validateYAMLSyntaxOrder' :: [Token.Token] -> [Error] -> IO (Error)
validateYAMLSyntaxOrder' [] [] = return Nothing

validateYAMLSyntaxOrder' (Token.Sharp : x : Token.NewLine : rest) errs = do
    case x of
        Token.Space -> validateYAMLSyntaxOrder' rest errs
        _ -> validateYAMLSyntaxOrder' rest (errs ++ []) -- space error

validateYAMLSyntaxOrder' tokens errs = do
    case tokens of
        (Token.Space _ : rest) -> validateWithSpace rest
        _ -> validateWithoutSpace tokens

    where
        validateWithSpace (Token.Scalar _ : x : Token.NewLine : rest) = do
            

        validateWithoutSpace (Token.Scalar _ : x : Token.NewLine : rest) = do
            

        validateWithSpace _ = return ()  -- Handle other cases or errors if needed
        validateWithoutSpace _ = return ()  -- Handle other cases or errors if needed
    
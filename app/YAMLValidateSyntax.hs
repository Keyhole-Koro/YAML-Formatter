module YAMLValidateSyntax where

import YAMLTokenizer
import Utils

import qualified Data.Token as Token
import qualified Data.Error as Error

validateYAMLSyntax :: [Token.Token] -> IO ()
validateYAMLSyntax tokens = do
    result <- validateYAMLSyntax' tokens
    case result of
        Just errorPos -> putStrLn $ "Syntax error at position " ++ show errorPos
        Nothing -> putStrLn "YAML syntax is correct"


validateYAMLSyntax' :: [Token.Token] -> IO (Maybe Int)
validateYAMLSyntax' [] = return Nothing
validateYAMLSyntax' (Token.Sharp:Token.Scalar msg:Token.NewLine:xs) = do
    putStrLn $ "Warning: Sharp followed by Scalar \"" ++ msg ++ "\""
    validateYAMLSyntax' xs
validateYAMLSyntax' (Token.Sharp:xs) = return $ Just 0 -- Error: Token.Sharp without Token.Scalar
validateYAMLSyntax' (Token.Scalar _:Token.Colon:Token.Space:xs) = validateYAMLSyntax' xs
validateYAMLSyntax' (Token.Scalar _:Token.Space:Token.Scalar _:Token.Space:Token.Sharp:Token.Scalar msg:Token.NewLine:xs) = do
    putStrLn $ "Syntax error: " ++ msg
    validateYAMLSyntax' xs
validateYAMLSyntax' (Token.Scalar _:Token.Space:Token.Scalar _:Token.NewLine:xs) = do
    putStrLn "Error: Missing Colon after key"
    validateYAMLSyntax' xs
validateYAMLSyntax' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:Token.Dash:Token.Space:_:xs) = validateYAMLSyntax' xs
validateYAMLSyntax' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:Token.Dash:Token.Space:Token.Scalar _:xs) = validateYAMLSyntax' xs
validateYAMLSyntax' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:Token.Dash:xs) = return $ Just 0 -- Error: Token.Scalar missing after Token.Dash
validateYAMLSyntax' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:_:xs) = do
    putStrLn "Error: Unexpected token after Dash"
    validateYAMLSyntax' xs
validateYAMLSyntax' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:Token.Space:Token.Scalar _:Token.Colon:Token.Space:_:xs) = validateYAMLSyntax' xs
validateYAMLSyntax' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:Token.Space:Token.Scalar _:Token.Colon:Token.Space:Token.Space:Token.Sharp:Token.Scalar msg:Token.NewLine:xs) = do
    putStrLn $ "Syntax error: " ++ msg
    validateYAMLSyntax' xs
validateYAMLSyntax' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:Token.Space:Token.Scalar _:Token.Colon:Token.Space:Token.Space:xs) = do
    putStrLn "Error: Missing Colon after key"
    validateYAMLSyntax' xs
validateYAMLSyntax' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:Token.Space:Token.Scalar _:Token.Colon:Token.Space:xs) = do
    putStrLn "Error: Missing value after Colon"
    validateYAMLSyntax' xs
validateYAMLSyntax' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:Token.Space:Token.Scalar _:xs) = do
    putStrLn "Error: Missing Colon after key"
    validateYAMLSyntax' xs
validateYAMLSyntax' (Token.Scalar _:Token.Colon:Token.NewLine:Token.Space:Token.Space:xs) = do
    putStrLn "Error: Missing value after Colon"
    validateYAMLSyntax' xs
validateYAMLSyntax' (Token.Scalar _:Token.Colon:Token.NewLine:xs) = return $ Just 0 -- Error: Token.Scalar missing after Token.Colon
validateYAMLSyntax' (Token.Scalar _:Token.NewLine:xs) = do
    putStrLn "Error: Missing Colon after key"
    validateYAMLSyntax' xs
validateYAMLSyntax' (Token.Colon:Token.Space:Token.Scalar _:xs) = validateYAMLSyntax' xs
validateYAMLSyntax' (Token.Colon:xs) = return $ Just 0 -- Error: Token.Colon without key
validateYAMLSyntax' (Token.Space:xs) = validateYAMLSyntax' xs
validateYAMLSyntax' (Token.Dash:Token.Space:Token.Scalar _:xs) = validateYAMLSyntax' xs
validateYAMLSyntax' (Token.Dash:xs) = return $ Just 0 -- Error: Token.Dash without Token.Scalar
validateYAMLSyntax' (Token.NewLine:xs) = validateYAMLSyntax' xs
validateYAMLSyntax' (Token.EOF:_) = return Nothing
validateYAMLSyntax' _ = return $ Just 0 -- Error: Unexpected token

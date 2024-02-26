module YAMLValidateSyntax where

import YAMLTokenizer
import Utils

import Token
import Error

validateSyntax :: [Token] -> [Error]
validateSyntax tokens = validateSyntax' tokens 1 []

validateSyntax' :: [Token] -> Int -> [Error] -> [Error]
validateSyntax' [] _ acc = acc
validateSyntax' (tkn:tokens) lineNum acc =
    case tkn of 
        Token.NewLine -> do
            let newErrors = validateIndentSpaces tokens lineNum acc
            validateSyntax' tokens (lineNum + 1) (acc ++ newErrors)
            
        _ -> validateSyntax' tokens lineNum acc

validateIndentSpaces :: [Token] -> Int -> Int -> [Error] -> [Error]
validateIndentSpaces tokens lineNum acc =
    let spaces = length (readWhile (== Token.Space) tokens)
        numOfSpaces = len spaces
    in if numOfSpaces /= 2
            then validateSyntax' Token.IndentSpaces lineNum (Error Space lineNum ("There are " ++ show numOfSpaces ++ " spaces") "The number of spaces of an indent is supposed to be 2" : acc)
            else []

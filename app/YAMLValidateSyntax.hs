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

spaceError :: Rank -> Int -> Int -> Int -> Error
spaceError rnk tknIndex spcs expectedSpcs errors =
    let errorMsg
            | spcs > expectedSpcs = "Excessive number of spaces"
            | spcs < expectedSpcs = "Insufficient number of spaces"
            | otherwise = "Correct number of spaces"
        newError = if errorMsg /= "Correct number of spaces"
                      then ([ErrorKind.Space
                            rnk
                            tknIndex
                            errorMsg
                            ("There is(are) " ++ show expectedSpcs ++ " space(s)") |
                            errorMsg /= "Correct number of spaces"])
                      else []
    in newError

excessError :: Int -> [Token] -> Error
excessError tknIndex excess =
    case excess of
        (Token.Space n : Token.Comment : _) -> spaceError Rank.Recomment tknIndex n 1
        (Token.Comment : _) -> spaceError Rank.Recomment tknIndex 0 1
        [] -> []
        _ -> [ErrorKind.Syntax Rank.Fatal tknIndex "There may be excess tokens" " "]


mappingError :: [Token.Token] -> Error

sequenceError :: [Token.Token] -> Error


validateYAMLSyntaxOrder' :: [Token.Token] -> Int -> Int -> [Error] -> [Error]
validateYAMLSyntaxOrder' _ _ _ errs = return $ Just $ Error.CustomError "Unhandled pattern in validation"

-- process comment
validateYAMLSyntaxOrder' (Token.Sharp : x : Token.NewLine : rest) tknIndex dpth errs = do
    case x of
        Token.Space n -> validateYAMLSyntaxOrder' rest dpth errs
        _ -> validateYAMLSyntaxOrder' rest dpth (errs ++ (spaceError Rank.Recomment tknIndex n dpth*2))

-- process initial key value
-- ex) name: Keyhole
validateYAMLSyntaxOrder' tokens@(Token.Scalar _ : Token.Colon : Token.Space n : scalar : excess : Token.NewLine : rest) tknIndex dpth errs
    | isScalarToken scalar = do
        let newErrors = spaceError Rank.Fatal tknIndex n (dpth * 2) : excessError tknIndex excess : errs
        validateYAMLSyntaxOrder' rest tknIndex dpth newErrors
    | otherwise = validateYAMLSyntaxOrder' tokens tknIndex dpth newErrors


validateYAMLSyntaxOrder' (Token.Scalar _ : Token.Colon : excess : Token.NewLine : rest) tknIndex dpth errs = do
    validateYAMLSyntaxItem 




validateYAMLSyntaxItem :: [Token.Token] -> Int -> Int -> [Error] -> [Error]
-- _ is space but doesnt consider the number of it
-- ex) _ - element1
validateYAMLSyntaxItem (Token.Space n1 : Token.Dash : Token.Space n2 : Token.Scalar _ : excess : Token.NewLine : rest ) tknIndex dpth errs = do

-- ex) _ key: value
validateYAMLSyntaxItem (Token.Space n1 : Token.Scalar _ : Token.Colon : Token.Space n : scalar : excess : Token.NewLine : rest) tknIndex dpth errs
    | isScalarToken scalar = do

-- ex) _ [element1, element2, element3]
validateYAMLSyntaxItem (Token.Space n1 : start : elements : end : excess : Token.NewLine : rest) tknIndex dpth errs
    | isStart start && isEnd end = do 
    | otherwise = 


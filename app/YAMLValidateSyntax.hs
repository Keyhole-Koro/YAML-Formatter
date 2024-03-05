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


-- supposed to receive the inside of mappings
mappingError :: [Token.Token] -> Int -> Error
mappingError [] tknIndex = [] -- success
mappingError token tknIndex =
    case token of
        -- scalar:scalar,
        (Token.Scalar _ : Token.Colon : scalar : Token.Comma : rest) ->
        -- scalar: _ scalar,
        (Token.Scalar _ : Token.Colon : : Token.Space n : scalar : Token.Comma : rest) ->
        -- _ scalar: _ scalar,
        (Token.Space n1 : Token.Scalar _ : Token.Colon : Token.Space n2 : scalar : Token.Comma :rest) -> -- n other than 0 or 1, recomendation error
        -- _ scalar _ : _ scalar,  
        (Token.Space n1 : Token.Scalar _ : Token.Space n2 : Token.Colon : Token.Space n3 : scalar : Token.Comma : rest) -> -- n2 other than 0, recomendation error
        _ -> -- fatal error

-- supposed to receive the inside of sequences
sequenceError :: [Token.Token] -> Int -> Error
sequenceError [] tknIndex = [] -- success
sequenceError token tknIndex =
    case token of
        (Token.Scalar _ : Token.Comma : rest) ->
        (Token.Space n : Token.Scalar _ : Token.Comma : rest) -> -- n other than 0 or 1, recomendation error
        (Token.Space n1 : Token.Scalar _ : Token.Space n2 : Token.Comma : rest) -> -- n2 other than 0, recomendation error
        _ -> -- fatal error

validateYAMLSyntaxOrder' :: [Token.Token] -> Int -> Int -> [Error] -> [Error]
validateYAMLSyntaxOrder' _ _ _ errs = return $ Just $ Error.CustomError "Unhandled pattern in validation"

-- process comment
validateYAMLSyntaxOrder' (Token.Sharp : x : Token.NewLine : rest) tknIndex dpth errs = do
    case x of
        Token.Space n -> validateYAMLSyntaxOrder' rest dpth errs
        _ -> validateYAMLSyntaxOrder' rest dpth (errs ++ (spaceError Rank.Recomment tknIndex n dpth*2))

-- process initial key value
-- key: value
validateYAMLSyntaxOrder' tokens@(Token.Scalar _ : Token.Colon : Token.Space n : scalar : excess : Token.NewLine : rest) tknIndex dpth errs
    | isScalarToken scalar = do
        let newErrors = spaceError Rank.Fatal tknIndex n (dpth * 2) : excessError tknIndex excess : errs
        validateYAMLSyntaxOrder' rest tknIndex dpth newErrors
    | otherwise = validateYAMLSyntaxOrder' tokens tknIndex dpth newErrors

-- key:
validateYAMLSyntaxOrder' (Token.Scalar _ : Token.Colon : excess : Token.NewLine : rest) tknIndex dpth errs = do
    validateYAMLSyntaxItem 



validateYAMLSyntaxItem :: [Token.Token] -> Int -> Int -> [Error] -> [Error]
-- _ is space but doesnt consider the number of it
-- _ - element1
validateYAMLSyntaxItem (Token.Space n1 : Token.Dash : Token.Space n2 : Token.Scalar _ : excess : Token.NewLine : rest ) tknIndex dpth errs = do

-- _ key:
validateYAMLSyntaxItem (Token.Space n1 : Token.Scalar _ : Token.Colon : excess : Token.NewLine : rest) tknIndex dpth errs


-- _ key: value
validateYAMLSyntaxItem (Token.Space n1 : Token.Scalar _ : Token.Colon : Token.Space n : scalar : excess : Token.NewLine : rest) tknIndex dpth errs
    | isScalarToken scalar = do

-- _ [element1, element2, element3]
validateYAMLSyntaxItem (Token.Space n1 : start : elements : end : excess : Token.NewLine : rest) tknIndex dpth errs
    | isStart start && isEnd end = do 
    | otherwise = 


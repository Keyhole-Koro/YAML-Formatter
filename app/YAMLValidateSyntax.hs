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
        (Token.Comment : _) -> spaceError Rank.Recommend tknIndex 0 1
        [] -> []
        _ -> [ErrorKind.Syntax Rank.Fatal tknIndex "There may be excess tokens" " "]

-- | Function to validate the inside of mappings
mappingError :: [Token.Token] -> Int -> [Error]
mappingError [] _ = [] -- success
mappingError tokens tknIndex = validateKeyValue tokens tknIndex

-- | Function to validate the syntax of key-value pairs
validateKeyValue :: [Token.Token] -> Int -> [Error]
validateKeyValue [] _ = []
validateKeyValue (Token.Scalar _ : Token.Colon : Token.Scalar : Token.Comma : rest) tknIndex =
    validateSpace rest (tknIndex + )
    where
        validateSpace :: [Token.Token] -> Int -> [Error]
        validateSpace [] _ = []
        validateSpace (Token.Space n : rest) = spaceError Rank.Recommend tknIndex n 1 : validateSpace rest tknIndex + 1
        validateSpace (Token.Scalar _ _ : rest) index = validateSpace rest (index + 1)
        validateSpace (Token.Colon : rest) index = validateSpace rest (index + 1)
        validateSpace (Token.Comma : rest) index = validateSpace rest (index + 1)
        validateSpace _ index = [fatalError] -- fatal error
validateKeyValue _ _ = [fatalError] -- fatal error


-- | Function to validate the inside of sequences
sequenceError :: [Token.Token] -> Int -> [Error]
sequenceError (Token.Scalar _ _ : []) _ = [] -- success
sequenceError (Token.Scalar _ _ : Token.Comma : rest) tknIndex = 
    validateSpace rest ++ sequenceError rest (tknIndex + 2)
    where
        -- allow 1 space
        validateSpace :: [Token.Token] -> tknIndex -> [Error]
        validateSpace [] = []
        validateSpace (Token.Scalar _ _ : rest) = validateSpace rest tknIndex + 1
        validateSpace (Token.Space n : rest) = spaceError Rank.Recommend tknIndex n 1 : validateSpace rest tknIndex + 1
        validateSpace (Token.Comma : rest) = validateSpace rest tknIndex + 1
        validateSpace _ = [fatalError] -- fatal error
        

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


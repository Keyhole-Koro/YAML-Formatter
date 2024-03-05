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


validateYAMLSyntaxOrder' :: [Token.Token] -> Int -> Int -> [Error] -> IO (Error)
validateYAMLSyntaxOrder' _ _ _ errs = return $ Just $ Error.CustomError "Unhandled pattern in validation"

-- process comment
validateYAMLSyntaxOrder' (Token.Sharp : x : Token.NewLine : rest) tknIndex dpth errs = do
    case x of
        Token.Space n -> validateYAMLSyntaxOrder' rest dpth errs
        _ -> validateYAMLSyntaxOrder' rest dpth (errs ++ (spaceError Rank.Recomment tknIndex n dpth*2))

-- process initial key value
validateYAMLSyntaxOrder' (Token.Scalar key : Token.Colon : Token.Space n : Token.QuotedScalar val : excess : Token.NewLine : rest) tknIndex dpth errs = do
    let newErrors = []
        newErrors ++ spaceError Rank.Fatal tknIndex n (dpth * 2) -- space error if its okey, it gives []
        newErrors ++ excessError tknIndex excess
    in validateYAMLSyntaxOrder' rest tknIndex dpth errs
    

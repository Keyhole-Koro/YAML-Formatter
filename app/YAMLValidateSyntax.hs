module YAMLValidateSyntax where

import YAMLTokenizer
import Utils

import qualified Data.Token as Tk

import Error.Error (Err(..))
import Error.ErrorKind as ErrKind
import Error.ErrorRank as ErrRank

spaceError :: ErrRank -> Int -> Int -> Tk.Tk
spaceError rnk spcs expectedSpcs
    | spcs > expectedSpcs = createSpaceError "Excessive number of spaces"
    | spcs < expectedSpcs = createSpaceError "Insufficient number of spaces"
    | otherwise           = Tk.Space spcs
    where
        createSpaceError :: String -> Tk.Tk
        createSpaceError errorMsg = Tk.Error (ErrRec ErrKind.Excess rnk (Tk.Space spcs) (Tk.Space expectedSpcs) errorMsg)

excessErrors :: [Tk.Tk] -> [Tk.Tk]
excessErrors excess =
    case excess of
        (Tk.Space _ : Tk.Comment _ : _) -> excess
        (Tk.Comment _ : _) -> excess
        _ -> createExcessError excess
    where
        createExcessError :: [Tk.Tk] -> [Tk.Tk]
        createExcessError [] = []
        createExcessError (tkn:rest) =
            Tk.Error
                (ErrRec ErrKind.Excess ErrRank.Fatal tkn Tk.Empty
                "This may be excess token") : createExcessError rest


-- | Function to validate the inside of sequences
-- element1, element2, element3 allows spaces (recommend error is occured)
validateSequence :: [Tk.Tk] -> [Tk.Tk]
validateSequence [] = [] -- Base case: empty list, no errors
validateSequence tokens = validateSequence' tokens (Tk.Scalar "" (-1))

validateSequence' :: [Tk.Tk] -> Tk.Tk -> [Tk.Tk]
validateSequence' [] _ = [] -- Base case: empty list, no errors
validateSequence' (tkn:rest) expectedTkn =
    case tkn of
        Tk.Space n -> spaceError ErrRank.Recommend n 1 : validateSequence' rest expectedTkn
        Tk.Scalar _ _ -> tkn : validateSequence' rest Tk.Comma
        Tk.Comma -> tkn : validateSequence' rest (Tk.Scalar "" 0)
        _ -> excessErrors [tkn] ++ validateSequence' rest expectedTkn





-- | Function to validate the inside of mappings
validateMapping :: [Tk.Token] -> [Tk.Token]
validateMapping [] = [] -- success
validateMapping (Token.Scalar _ : Token.Colon : Token.Scalar : Token.Comma : rest) = rest
validateMapping tokens =  : tokens
    where
        validateSpace :: [Tk.Token] -> [Tk.Token]
        validateSpace [] _ = []
        validateSpace (Token.Space n : rest) = spaceError Rank.Recommend n 1 : validateSpace rest
        validateSpace (Token.Scalar _ _ : rest) index = validateSpace rest
        validateSpace (Token.Colon : rest) index = validateSpace rest
        validateSpace (Token.Comma : rest) index = validateSpace rest
        validateSpace _ index =  -- fatal error

validateSequence token tknIndex =
    case token of
        (Token.Scalar _ : Token.Comma : rest) ->
        (Token.Space n : Token.Scalar _ : Token.Comma : rest) -> -- n other than 0 or 1, recomendation error
        (Token.Space n1 : Token.Scalar _ : Token.Space n2 : Token.Comma : rest) -> -- n2 other than 0, recomendation error
        _ -> -- fatal error



validateYAMLSyntax :: [Tk.Token] -> IO ()

validateYAMLSyntaxOrder' :: [Tk.Token] -> Int -> Int -> [Error] -> [Error]
validateYAMLSyntaxOrder' _ _ _ errs = return $ Just $ Error.CustomError "Unhandled pattern in validation"

-- process comment
validateYAMLSyntaxOrder' (Token.Sharp : x : Token.NewLine : rest) tknIndex dpth errs = do
    case x of
        Token.Space n -> validateYAMLSyntaxOrder' rest dpth errs
        _ -> validateYAMLSyntaxOrder' rest dpth (errs ++ (spaceError Rank.Recomment tknIndex n dpth*2))

-- process initial key value
-- key: value
validateYAMLSyntaxOrder' tokens@(Token.Scalar _ : Token.Colon : Token.Space n : Token.Scalar str m : excess : Token.NewLine : rest) tknIndex dpth errs
    | isScalarToken scalar = do
        let newErrors = spaceError Rank.Fatal tknIndex n (dpth * 2) : excessError tknIndex excess : errs
        validateYAMLSyntaxOrder' rest tknIndex dpth newErrors
    | otherwise = validateYAMLSyntaxOrder' tokens tknIndex dpth newErrors

-- key:
validateYAMLSyntaxOrder' (Token.Scalar _ : Token.Colon : excess : Token.NewLine : rest) tknIndex dpth errs = do
    validateYAMLSyntaxItem 

validateYAMLSyntaxItem :: [Tk.Token] -> Int -> Int -> [Error] -> [Error]
-- _ is space but doesnt consider the number of it
-- _ - element1
validateYAMLSyntaxItem (Token.Space n1 : Token.Dash : Token.Space n2 : Token.Scalar _ : excess : Token.NewLine : rest ) tknIndex dpth errs = do

-- _ key:
validateYAMLSyntaxItem (Token.Space n1 : Token.Scalar _ : Token.Colon : excess : Token.NewLine : rest) tknIndex dpth errs


-- _ key: value
validateYAMLSyntaxItem (Token.Space n1 : Token.Scalar _ : Token.Colon : Token.Space n : Token.Scalar str m : excess : Token.NewLine : rest) tknIndex dpth errs
    | isScalarToken scalar = do

-- _ [element1, element2, element3]
validateYAMLSyntaxItem (Token.Space n1 : start : elements : end : excess : Token.NewLine : rest) tknIndex dpth errs
    | isStart start && isEnd end = do 
    | otherwise = 


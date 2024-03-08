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

scalarKeyError :: Tk.Tk -> Tk.Tk
scalarKeyError tkn =
    case tkn of
        Tk.Scalar str n -> if isScalarToken n
            then tkn
            else Tk.Err (ErrRec ErrKind.Key ErrRank.Recommend tkn (Tk.Scalar str 0) "This is not supposed to be quoted")
        _ -> Tk.Err (ErrRec ErrKind.ImplementationError ErrRank.ImplementationError Tk.Empty Tk.Empty "this is not sopposed to happen")

excessQuotationError :: Tk.Tk -> Tk.Tk
excessQuotationError excess =
    Tk.Error (ErrRec ErrKind.Excess ErrRank.Fatal excess Tk.Empty "The quotation is not needed")


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
        Tk.Scalar _ _ -> 
            case expectedTkn of
                Tk.Scalar _ _ -> tkn : validateSequence' rest Tk.Comma
                _ -> excessErrors [tkn] ++ validateSequence' rest expectedTkn
                
        Tk.Comma -> 
            case expectedTkn of
                Tk.Comma -> tkn : validateSequence' rest (Tk.Scalar "" 0)
                _ -> excessErrors [tkn] ++ validateSequence' rest expectedTkn

        _ -> excessErrors [tkn] ++ validateSequence' rest expectedTkn

-- Define spaceError and excessErrors functions
-- These functions are assumed to be defined elsewhere in your codebase


-- | Function to validate the inside of mappings
-- key: value, key2: value2
validateMapping :: [Tk.Tk] -> [Tk.Tk]
validateMapping [] = [] -- success
validateMapping tokens = validateMapping' tokens (Tk.Scalar _ _) Tk.Colon

validateMapping' :: [Tk.Tk] -> Tk.Tk -> Tk.Tk -> [Tk.Tk]
validateMapping' [] _ = []
validateMapping' (tkn:rest) expectedTkn nextTkn =
    case tkn of
        Tk.Space n -> spaceError ErrRank.Recommend n 1 : validateSequence' rest expectedTkn
        Tk.Scalar _ n -> if n == 0 && nextTkn == Tk.Colon
            then tkn : validateMapping' rest (Tk.Scalar _ _) Tk.Comma
            else if nextTkn == Tk.Comma
            then tkn : validateMapping' rest (Tk.Scalar _ _) Tk.Colon
            else excessQuotationError tkn : validateMapping' rest 
        Tk.Comma -> tkn : validateMapping' rest (Tk.Scalar _ _) Tk.Colon
        Tk.Colon -> tkn : validateMapping' rest (Tk.Scalar _ _) Tk.Comma
        _ -> excessErrors [tkn] ++ validateSequence' rest expectedTkn


validateYAMLSyntax :: [Tk.Tk] -> IO ()

validateYAMLSyntaxOrder' :: [Tk.Tk] -> [Tk.Tk]
validateYAMLSyntaxOrder' [] _ = []

-- process initial key value
-- key: value
validateYAMLSyntaxOrder' (Tk.Scalar str sn: Tk.Colon : Tk.Space n : Tk.Scalar str2 m : excess : Tk.NewLine : rest) = 
    scalarKeyError (Tk.Scalar str sn) : Tk.Colon : spaceError (ErrRank.Space n 1) : Tk.Scalar str3 m : excessError excess : Tk.NewLine : validateYAMLSyntaxItem rest 1

-- key:
validateYAMLSyntaxOrder' (Tk.Scalar str sn : Tk.Colon : excess : Tk.NewLine : rest) =
    Tk.Scalar str sn : Tk.Colon : excessError excess : Tk.NewLine : validateYAMLSyntaxItem rest 1


validateYAMLSyntaxItem :: [Tk.Tk] -> [Tk.Tk]

-- _ is space but doesnt consider the number of it
-- _ - element1
validateYAMLSyntaxItem (Tk.Space n1 : Tk.Dash : Tk.Space n2 : Tk.Scalar str n : excess : Tk.NewLine : rest ) depth =
    spaceError ErrRank.Fatal n1 (depth*2) : Tk.Dash : spaceError ErrRank.Recommend n2 1 : Tk.Scalar str n : excessError excess : Tk.NewLine : validateYAMLSyntaxItem rest depth+1

-- _ key:
validateYAMLSyntaxItem (Tk.Space n1 : Tk.Scalar str n : Tk.Colon : excess : Tk.NewLine : rest) depth =
    spaceError ErrRank.Fatal n1 (depth*2) : scalarKeyError (Tk.Scaar str n) : Tk.Colon : excessError excess : Tk.NewLine : validateYAMLSyntaxItem rest depth+1


-- _ [element1, element2, element3]

-- _ {key1:value1, key2:value2, key3:value3}

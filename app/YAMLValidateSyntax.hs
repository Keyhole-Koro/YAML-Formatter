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

-- add block scalar processing
excessErrors :: [Tk.Tk] -> [Tk.Tk]
excessErrors excess =
    case excess of
        (Tk.Space _ : Tk.Comment _ : _) -> excess
        (Tk.Comment _ : _) -> excess
        _ -> createexcessErrors excess
    where
        createexcessErrors :: [Tk.Tk] -> [Tk.Tk]
        createexcessErrors [] = []
        createexcessErrors (tkn:rest) =
            Tk.Error
                (ErrRec ErrKind.Excess ErrRank.Fatal tkn Tk.Empty
                "This may be excess token") : createexcessErrors rest

scalarKeyError :: Tk.Tk -> Tk.Tk
scalarKeyError tkn =
    case tkn of
        Tk.Scalar str st -> if isScalarToken st
            then tkn
            else Tk.Error (ErrRec ErrKind.Key ErrRank.Recommend tkn (Tk.Scalar str 0) "This is not supposed to be quoted")
        _ -> Tk.Error (ErrRec ErrKind.ImplementationError ErrRank.ImplementationError Tk.Empty Tk.Empty "this is not supposed to happen")

excessQuotationError :: Tk.Tk -> Tk.Tk
excessQuotationError excess =
    Tk.Error (ErrRec ErrKind.Excess ErrRank.Fatal excess Tk.Empty "The quotation is not needed")

invalidError :: Tk.Tk -> Tk.Tk
invalidError invalid =
    Tk.Error (ErrRec ErrKind.Invalid ErrRank.Fatal invalid Tk.Empty "This token is unnecessary for this syntax")

-- | Function to validate the inside of sequences
-- element1, element2, element3 allows spaces (recommend error is occurred)
validateSequence :: [Tk.Tk] -> [Tk.Tk]
validateSequence [] = [] -- Base case: empty list, no errors
validateSequence tokens = validateSequence' tokens (Tk.Scalar "" St.Empty)

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
                Tk.Comma -> tkn : validateSequence' rest (Tk.Scalar "" St.NoQuote)
                _ -> excessErrors [tkn] ++ validateSequence' rest expectedTkn
        _ -> excessErrors [tkn] ++ validateSequence' rest expectedTkn

-- | Function to validate the inside of mappings
-- key: value, key2: value2
validateMapping :: [Tk.Tk] -> [Tk.Tk]
validateMapping [] = [] -- success
validateMapping tokens = validateMapping' tokens (Tk.Scalar "" St.Empty) Tk.Colon

validateMapping' :: [Tk.Tk] -> Tk.Tk -> Tk.Tk -> [Tk.Tk]
validateMapping' [] _ _ = []
validateMapping' (tkn:rest) expectedTkn nextTkn =
    case tkn of
        Tk.Space n -> spaceError ErrRank.Recommend n 1 : validateMapping' rest expectedTkn nextTkn
        Tk.Scalar _ n ->
            case expectedTkn of
                Tk.Scalar _ _ ->
                    if n == 0 && nextTkn == Tk.Colon -- key
                        then tkn : validateMapping' rest Tk.Colon (Tk.Scalar "" St.Empty)
                        else if nextTkn == Tk.Comma -- value
                            then tkn : validateMapping' rest Tk.Comma (Tk.Scalar "" St.Empty)
                            else excessQuotationError tkn : validateMapping' rest Tk.Comma (Tk.Scalar "" (-1))
                _ -> invalidError tkn : validateMapping' rest expectedTkn nextTkn
        Tk.Comma ->
            case expectedTkn of
                Tk.Comma -> tkn : validateMapping' rest (Tk.Scalar "" St.Empty) Tk.Colon
                _ -> invalidError tkn : validateMapping' rest expectedTkn nextTkn
        Tk.Colon ->
            case tkn of
                Tk.Colon -> tkn : validateMapping' rest (Tk.Scalar "" St.Empty) Tk.Comma
                _ -> invalidError tkn : validateMapping' rest expectedTkn nextTkn
        _ -> excessErrors [tkn] ++ validateMapping' rest expectedTkn nextTkn

-- Validate YAML Syntax Order
validateYAMLSyntaxOrder' :: [Tk.Tk] -> Int -> [Tk.Tk]
validateYAMLSyntaxOrder' [] _ = []

-- key: value
validateYAMLSyntaxOrder' (Tk.Scalar str st1 : Tk.Colon : Tk.Space n : Tk.Scalar str2 st2 : excess : Tk.NewLine : rest) _ =
    scalarKeyError (Tk.Scalar str st1) : Tk.Colon : spaceError ErrRank.Recommend n 1 : Tk.Scalar str2 st2 : excessErrors [excess] ++ [Tk.NewLine] ++ validateYAMLSyntaxOrder' rest 1

-- key:
validateYAMLSyntaxOrder' (Tk.Scalar str st : Tk.Colon : excess : Tk.NewLine : rest) _ =
    Tk.Scalar str st : Tk.Colon : excessErrors [excess] ++ [Tk.NewLine] ++ validateYAMLSyntaxOrder' rest 1

-- Validate YAML Syntax Item
validateYAMLSyntaxItem :: [Tk.Tk] -> Int -> [Tk.Tk]

-- _ - element
validateYAMLSyntaxItem (Tk.Space n1 : Tk.Dash : Tk.Space n2 : Tk.Scalar str st : excess : Tk.NewLine : rest) depth =
    spaceError ErrRank.Fatal n1 (depth*2) : Tk.Dash : spaceError ErrRank.Recommend n2 1 : Tk.Scalar str st : excessErrors [excess] ++ [Tk.NewLine] ++ validateYAMLSyntaxItem rest depth

-- _ -
validateYAMLSyntaxItem (Tk.Space n1 : Tk.Dash : excess : Tk.NewLine : rest) depth =
    spaceError ErrRank.Fatal n1 (depth*2) : Tk.Dash : excessErrors [excess] ++ [Tk.NewLine] ++ validateYAMLSyntaxItem rest (depth + 1)

-- _ key: value

-- variable existence


-- key: |
validate

-- key: >
validateYAMLSyntaxItem tokens@(tkn:rest) depth =
    case tkn of
        Tk.Space n -> 
        Tk.Scalar str st -> validateYAMLSyntaxOrder' tokens
        _ -> excessErrors [tkn] : validateYAMLSyntaxItem rest
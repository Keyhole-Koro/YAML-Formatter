module YAMLValidateSyntax where

import Data.List (nub)

import YAMLTokenizer
import Utils
import Stack

import Token

import Error.Error
import Error.ErrorKind
import Error.ErrorRank

spaceError :: ErrRank -> Int -> Int -> Token
spaceError rnk spcs expectedSpcs
    | spcs > expectedSpcs = createSpaceError "Excessive number of spaces"
    | spcs < expectedSpcs = createSpaceError "Insufficient number of spaces"
    | otherwise           = Kind.Space spcs
    where
        createSpaceError :: String -> Token
        createSpaceError errorMsg = Kind.Error (ErrRec ErrKind.Excess rnk (Kind.Space spcs) (Kind.Space expectedSpcs) errorMsg)

validateYAMLSpace :: [Token] -> [Token]
validateYAMLSpace



-- add block scalar processing
excessErrors :: [Token] -> [Token]
excessErrors excess =
    case excess of
        (Kind.Space _ : Kind.Comment _ : _) -> excess
        (Kind.Comment _ : _) -> excess
        _ -> createexcessErrors excess
    where
        createexcessErrors :: [Token] -> [Token]
        createexcessErrors [] = []
        createexcessErrors (tkn:rest) =
            Kind.Error
                (ErrRec ErrKind.Excess ErrRank.Fatal tkn Kind.Empty
                "This may be excess token") : createexcessErrors rest

scalarKeyError :: Token -> Token
scalarKeyError tkn =
    case tkn of
        Kind.Scalar str st -> if isScalarToken st
            then tkn
            else Kind.Error (ErrRec ErrKind.Key ErrRank.Recommend tkn (Kind.Scalar str 0) "This is not supposed to be quoted")
        _ -> Kind.Error (ErrRec ErrKind.ImplementationError ErrRank.ImplementationError Kind.Empty Kind.Empty "this is not supposed to happen")

excessQuotationError :: Token -> Token
excessQuotationError excess =
    Kind.Error (ErrRec ErrKind.Excess ErrRank.Fatal excess Kind.Empty "The quotation is not needed")

invalidError :: Token -> Token
invalidError invalid =
    Kind.Error (ErrRec ErrKind.Invalid ErrRank.Fatal invalid Kind.Empty "This token is unnecessary for this syntax")

-- | Function to validate the inside of sequences
-- element1, element2, element3 allows spaces (recommend error is occurred)
validateSequence :: [Token] -> [Token]
validateSequence [] = [] -- Base case: empty list, no errors
validateSequence tokens = validateSequence' tokens (Kind.Scalar "" St.Empty)

validateSequence' :: [Token] -> Token -> [Token]
validateSequence' [] _ = [] -- Base case: empty list, no errors
validateSequence' (tkn:rest) expectedTkn =
    case tkn of
        Kind.Space n -> spaceError ErrRank.Recommend n 1 : validateSequence' rest expectedTkn
        Kind.Scalar _ _ -> 
            case expectedTkn of
                Kind.Scalar _ _ -> tkn : validateSequence' rest Kind.Comma
                _ -> excessErrors [tkn] ++ validateSequence' rest expectedTkn
        Kind.Comma -> 
            case expectedTkn of
                Kind.Comma -> tkn : validateSequence' rest (Kind.Scalar "" St.NoQuote)
                _ -> excessErrors [tkn] ++ validateSequence' rest expectedTkn
        _ -> excessErrors [tkn] ++ validateSequence' rest expectedTkn

-- | Function to validate the inside of mappings
-- key: value, key2: value2
validateMapping :: [Token] -> [Token]
validateMapping [] = [] -- success
validateMapping tokens = validateMapping' tokens (Kind.Scalar "" St.Empty) Kind.Colon

validateMapping' :: [Token] -> Token -> Token -> [Token]
validateMapping' [] _ _ = []
validateMapping' (tkn:rest) expectedTkn nextTkn =
    case tkn of
        Kind.Space n -> spaceError ErrRank.Recommend n 1 : validateMapping' rest expectedTkn nextTkn
        Kind.Scalar _ n ->
            case expectedTkn of
                Kind.Scalar _ _ ->
                    if n == 0 && nextTkn == Kind.Colon -- key
                        then tkn : validateMapping' rest Kind.Colon (Kind.Scalar "" St.Empty)
                        else if nextTkn == Kind.Comma -- value
                            then tkn : validateMapping' rest Kind.Comma (Kind.Scalar "" St.Empty)
                            else excessQuotationError tkn : validateMapping' rest Kind.Comma (Kind.Scalar "" (-1))
                _ -> invalidError tkn : validateMapping' rest expectedTkn nextTkn
        Kind.Comma ->
            case expectedTkn of
                Kind.Comma -> tkn : validateMapping' rest (Kind.Scalar "" St.Empty) Kind.Colon
                _ -> invalidError tkn : validateMapping' rest expectedTkn nextTkn
        Kind.Colon ->
            case tkn of
                Kind.Colon -> tkn : validateMapping' rest (Kind.Scalar "" St.Empty) Kind.Comma
                _ -> invalidError tkn : validateMapping' rest expectedTkn nextTkn
        _ -> excessErrors [tkn] ++ validateMapping' rest expectedTkn nextTkn

-- Validate YAML Syntax Order
validateYAMLSyntaxOrder' :: [Token] -> Int -> [Token]
validateYAMLSyntaxOrder' [] _ = []

-- key: value
validateYAMLSyntaxOrder' (Kind.Scalar str st1 : Kind.Colon : Kind.Space n : Kind.Scalar str2 st2 : excess : Kind.NewLine : rest) _ =
    scalarKeyError (Kind.Scalar str st1) : Kind.Colon : spaceError ErrRank.Recommend n 1 : Kind.Scalar str2 st2 : excessErrors [excess] ++ [Kind.NewLine] ++ validateYAMLSyntaxOrder' rest 1

-- key:
validateYAMLSyntaxOrder' (Kind.Scalar str st : Kind.Colon : excess : Kind.NewLine : rest) _ =
    Kind.Scalar str st : Kind.Colon : excessErrors [excess] ++ [Kind.NewLine] ++ validateYAMLSyntaxOrder' rest 1

-- Validate YAML Syntax Item
validateYAMLSyntaxItem :: [Token] -> Int -> [Token]

-- _ - element
validateYAMLSyntaxItem (Kind.Space n1 : Kind.Dash : Kind.Space n2 : Kind.Scalar str st : excess : Kind.NewLine : rest) depth =
    spaceError ErrRank.Fatal n1 (depth*2) : Kind.Dash : spaceError ErrRank.Recommend n2 1 : Kind.Scalar str st : excessErrors [excess] ++ [Kind.NewLine] ++ validateYAMLSyntaxItem rest depth

-- _ -
validateYAMLSyntaxItem (Kind.Space n1 : Kind.Dash : excess : Kind.NewLine : rest) depth =
    spaceError ErrRank.Fatal n1 (depth*2) : Kind.Dash : excessErrors [excess] ++ [Kind.NewLine] ++ validateYAMLSyntaxItem rest (depth + 1)

-- _ key: value

-- variable existence


-- key: |
validate

-- key: >
validateYAMLSyntaxItem tokens@(tkn:rest) depth =
    case tkn of
        Kind.Space n -> 
        Kind.Scalar str st -> validateYAMLSyntaxOrder' tokens
        _ -> excessErrors [tkn] : validateYAMLSyntaxItem rest



validateYAMLSyntaxItem (Kind.LiteralBlockStart st : rest) depth
    = do





processTokens (Kind.LiteralBlockStart : rest) = Kind.LiteralBlockStart : processLiteralBlock rest
processTokens (Kind.FoldedBlockStart : rest) = Kind.FoldedBlockStart : processFoldedBlock rest
processTokens (token : rest) = token : processTokens rest

processLiteralBlock :: [Kind.Token] -> [Kind.Token]
processLiteralBlock tokens =
    let (block, remainingTokens) = break (== Kind.LiteralBlockEnd) tokens
    in block ++ [Kind.LiteralBlockEnd] ++ processTokens (drop 1 remainingTokens)

processFoldedBlock :: [Kind.Token] -> [Kind.Token]
processFoldedBlock tokens =
    let (block, remainingTokens) = break (== Kind.FoldedBlockEnd) tokens
    in block ++ [Kind.FoldedBlockEnd] ++ processTokens (drop 1 remainingTokens)
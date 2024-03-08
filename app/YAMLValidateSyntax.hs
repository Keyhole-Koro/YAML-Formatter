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
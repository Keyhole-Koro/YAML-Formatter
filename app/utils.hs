module Utils where

import System.IO
import Data.Char (isSpace)

import Data.Token (Token(..), Kind(..))
import qualified Data.ScalarTypes as ST

import Error.Error (Err)
import Error.ErrorKind (ErrKind)
import Error.ErrorRank (ErrRank)

readWhile :: (Char -> Bool) -> Handle -> IO String
readWhile p handle = do
    char <- hLookAhead handle
    if p char
        then do
            _ <- hGetChar handle
            rest <- readWhile p handle
            return (char : rest)
        else return []

isScalarChar :: Char -> Bool
isScalarChar c = not (isSpace c || c `elem` [':', '-', '"', '\n', '\r', '\t'])

isScalarToken :: ST.ST -> Bool
isScalarToken  = st == ST.NoQuote

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' n (x : xs)
  | n == x = True
  | otherwise = elem' n xs

isStart :: Token -> Bool
isStart Token.SequenceStart = True
isStart Token.MappingStart = True
isStart _ = False

isEnd :: Token -> Bool
isEnd Token.SequenceEnd = True
isEnd Token.MappingEnd = True
isEnd _ = False

spaceStr :: Int -> String
spaceStr num_spaces = spaceStr' num_spaces ""
    where
        spaceStr' :: Int -> String -> String
        spaceStr' 0 str = str
        spaceStr' n str = spaceStr' (n - 1) (str ++ " ")

getLastChar :: String -> Maybe Char
getLastChar [] = Nothing
getLastChar str = Just (last str)
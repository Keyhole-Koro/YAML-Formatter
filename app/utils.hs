module Utils where

import System.IO
import Data.Char (isSpace)

import qualified Data.Token as Token

readWhile :: (Char -> Bool) -> Handle -> IO String
readWhile p handle = do
    char <- hLookAhead handle
    if p char
        then do
            _ <- hGetChar handle
            rest <- readWhile p handle
            return (char : rest)
        else return []

isTokenRegular :: [Token.Token] -> [Token.Token] -> Bool
isTokenRegular [] [] = True
isTokenRegular (x:xs) (y:ys) = x == y && isTokenRegular xs ys  -- 両方のリストの先頭が等しい場合、残りの要素について再帰的に比較
isTokenRegular _ _ = False

        
isScalarChar :: Char -> Bool
isScalarChar c = not (isSpace c || c `elem` [':', '-', '"', '\n', '\r', '\t'])

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' n (x : xs)
  | n == x = True
  | otherwise = elem' n xs

isScalarToken :: Token.Token -> Bool
isScalarToken (Token.SingleQuotedScalar _) = True
isScalarToken (Token.DoubleQuotedScalar _) = True
isScalarToken (Token.Scalar _) = True
isScalarToken _ = False

isStart :: Token.Token -> Bool
isStart (Token.MappingStart _) = True
isStart (Token.MappingStart _) = True
isStart _ = False

isEnd :: Token.Token -> Bool
isEnd (Token.MappingEnd _) = True
isEnd (Token.MappingEnd _) = True
isEnd _ = False

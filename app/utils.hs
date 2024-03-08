module Utils where

import System.IO
import Data.Char (isSpace)

import qualified Data.Token as Tk

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

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' n (x : xs)
  | n == x = True
  | otherwise = elem' n xs

isStart :: Tk.Tk -> Bool
isStart Tk.SequenceStart = True
isStart Tk.MappingStart = True
isStart _ = False

isEnd :: Tk.Tk -> Bool
isEnd Tk.SequenceEnd = True
isEnd Tk.MappingEnd = True
isEnd _ = False

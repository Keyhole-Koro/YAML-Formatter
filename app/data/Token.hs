module Data.Token where

import qualified Error.Error as Error

data Tk = Scalar String Int
           | Key String
           | Value String
           | Item String
           | Comment String
           | Space Int
           | MappingStart
           | MappingEnd
           | SequenceStart
           | SequenceEnd
           | Colon
           | Comma
           | Dash
           | NewLine
           | Sharp
           | EOF
           deriving (Show, Eq)

data Token = TokenRec { token :: Tk, lineNum :: Int, tokenIndex :: Int } deriving (Show, Eq)

type LineNumber = Int
type TokenIndex = Int

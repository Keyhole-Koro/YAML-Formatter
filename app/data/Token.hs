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
           | Empty
           deriving (Show, Eq)

data Token = TokenRec { token :: Tk, error :: Error.Err}

type LineNumber = Int
type TokenIndex = Int

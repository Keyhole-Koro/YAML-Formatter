module Data.Token where

import qualified Data.Error as Error

data Token = Scalar String Int
           | Key String
           | Value String
           | Item String
           | Comment String
           | MappingStart
           | MappingEnd
           | SequenceStart
           | SequenceEnd
           | Colon
           | Comma
           | Dash
           | NewLine
           | Space Int
           | Sharp
           | EOF
           | Error
           deriving (Show, Eq)
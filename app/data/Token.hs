module Data.Token (Token(..), Kind(..)) where

import Data.ScalarTypes (ST, BS)
import Error.Error (Err)

data Kind = Scalar String ST
           | Key String
           | Value String
           | Item String
           | Comment String
           | Space Int
           | LiteralBlockStart String BS -- | |+ |-
           | FoldedBlockStart String BS -- > >+ >-
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
           | Error Err
           | Empty
           deriving (Eq)

data Token = Token {
    kind :: Kind,
    line :: Int,
    line :: Int
    } deriving (Show)


instance Show Token where
    show (Scalar str st) = "Scalar " ++ show str ++ " " ++ show st
    show (Key str) = "Key " ++ show str
    show (Value str) = "Value " ++ show str
    show (Item str) = "Item " ++ show str
    show (Comment str) = "Comment " ++ show str
    show (Space int) = "Space " ++ show int
    show MappingStart = "MappingStart"
    show MappingEnd = "MappingEnd"
    show SequenceStart = "SequenceStart"
    show SequenceEnd = "SequenceEnd"
    show Colon = "Colon"
    show Comma = "Comma"
    show Dash = "Dash"
    show NewLine = "NewLine"
    show Sharp = "Sharp"
    show EOF = "EOF"
    show Empty = "Empty"
    show (Error err) = "!!!Error!!! " ++ show err

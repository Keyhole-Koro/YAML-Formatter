module Data.ScalarType where

data ST = NoQuote | SingleQuote | DoubleQuote | LiteralBlock | FoldedBlock | Empty deriving (Show, Eq)
module Data.ScalarType where

data ST = NoQuote
    | SingleQuote
    | DoubleQuote
    | LiteralBlock
    | LiteralBlockPlus
    | LiteralBlockMinus
    | FoldedBlock
    | FoldedBlockPlus
    | FoldedBlockMinus
    | Empty deriving (Show, Eq)
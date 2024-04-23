module Data.ScalarType where

-- scalar type
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

-- block scalar type
data BS = Plus
    | Minus
    | Enpty deriving (Show, Eq)
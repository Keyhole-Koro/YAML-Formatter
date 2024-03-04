module Data.Error where

data ErrorKind = ExcessSpaces
                | Space
                | Syntax
                | Comma
                | SquareBracket
                | DoubleSquotation
                deriving (Show, Eq)

data Rank = Fatal | Recommend

-- the second parameter is the index where error occured on a token list
-- the third parameter is an error message
-- the forth parameter is a sub error message
data Error = Error ErrorKind Rank Int String String deriving (Show, Eq)
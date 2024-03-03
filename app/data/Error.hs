module Data.Error where

data ErrorKind = ExcessSpaces
                | IndentSpaces
                | Space
                | Comma
                | SquareBracket
                | DoubleSquotation
                deriving (Show, Eq)

-- the second parameter is the index where error occured on a token list
-- the third parameter is an error message
-- the forth parameter is a sub error message
data Error = Error ErrorKind Int String String deriving (Show, Eq)
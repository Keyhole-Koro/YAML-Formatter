module Error.ErrorKind where
    
data ErrKind = ExcessSpaces
            | Space
            | Syntax
            | Excess -- shouldn't exist one
            | Comma
            | SquareBracket
            | DoubleQuotation
            | NoError  -- New constructor for absence of error
            deriving (Show, Eq)

module Error.ErrorKind where
    
data ErrKind = ExcessSpaces
            | Space
            | Syntax
            | Excess -- shouldn't exist one
            | Comma
            | SquareBracket
            | DoubleQuotation
            | Dummy
            deriving (Show, Eq)

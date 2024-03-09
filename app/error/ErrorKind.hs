module Error.ErrorKind where
    
data ErrKind = ExcessSpaces
            | Space
            | Syntax
            | Invalid
            | Key
            | Excess -- shouldn't exist one
            | Comma
            | SquareBracket
            | DoubleQuotation
            | ImplementationError
            deriving (Show, Eq)

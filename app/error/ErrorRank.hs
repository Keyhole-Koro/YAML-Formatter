module Error.ErrorRank (ErrRank(..)) where
    
data ErrRank = Fatal | Recommend | ImplementationError deriving (Show, Eq)

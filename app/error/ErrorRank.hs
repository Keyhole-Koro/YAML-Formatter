module Error.ErrorRank where
    
data ErrRank = Fatal | Recommend | ImplementationError deriving (Show, Eq)

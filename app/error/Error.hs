module Error.Error where

import Error.ErrorKind (ErrKind)
import Error.ErrorRank (ErrRank)

import {-# SOURCE #-} Data.Token(Tk)

-- ErrorKind Rank oldToken newToken ErrorMessage SubErrorMessage
data Err = ErrRec { kind :: ErrKind, rank :: ErrRank, newToken :: Tk, description :: Desc, information :: Info }

type Desc = String
type Info = String
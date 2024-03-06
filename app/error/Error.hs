module Error.Error where

import Error.ErrorKind (ErrKind)
import Error.ErrorRank (ErrRank)

import {-# SOURCE #-} Data.Token(Tk)

-- ErrorKind Rank oldToken newToken ErrorMessage SubErrorMessage
data Err = ErrRec { kind :: ErrKind, rank :: ErrRank, line :: Tk.LineNumber, tknIdx :: Tk.LokenIdx, newToken :: Tk, desciption :: Desc, infomation :: Info }

type Desc = String
type Info = String
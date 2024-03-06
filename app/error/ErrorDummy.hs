module Error.ErrorDummy (dummyErr) where

import Error.Error (Err(..))
import Error.ErrorKind as ErrKind
import Error.ErrorRank as ErrRank

dummyErr :: Err
dummyErr = ErrRec { kind = ErrKind.Dummy, rank = ErrRank.Dummy, newToken = Tk.Empty, description = "Dummy error", information = "This is a dummy error for initial setup" }

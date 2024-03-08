module Error.Error (Err(..)) where

import Error.ErrorKind (ErrKind)
import Error.ErrorRank (ErrRank)

import {-# SOURCE #-} Data.Token(Tk(..))

type Desc = String

data Err = ErrRec ErrKind ErrRank Tk Tk Desc deriving (Eq)

instance Show Err where
    show (ErrRec kind rank tk1 tk2 desc) = "ErrRec " ++ show kind ++ " "++ show rank ++ " " ++ show tk1 ++ " " ++ show tk2 ++ " " ++ "Desc: " ++ show desc

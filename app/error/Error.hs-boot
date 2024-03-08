{-# LANGUAGE FlexibleInstances #-}

module Error.Error (Err(..)) where

import Error.ErrorKind()
import Error.ErrorRank()

import {-# SOURCE #-} Data.Token()

-- ErrorKind Rank oldToken newToken ErrorMessage
data Err

instance Show Err
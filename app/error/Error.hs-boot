{-# LANGUAGE FlexibleInstances #-}

module Error.Error (Err(..)) where

import Error.ErrorKind()
import Error.ErrorRank()

import {-# SOURCE #-} Token()

-- ErrorKind Rank oldToken newToken ErrorMessage
data Err

instance Show Err
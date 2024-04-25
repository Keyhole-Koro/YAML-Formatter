module Stack (Stack, newStack, push, pop, peek) where

import Data.IORef

type Stack a = IORef [a]

newStack :: IO (Stack a)
newStack = newIORef []

push :: a -> Stack a -> IO ()
push x stack = modifyIORef stack (x :)

pop :: Stack a -> IO (Maybe a)
pop stack = do
    xs <- readIORef stack
    case xs of
        [] -> return Nothing
        (x:xs') -> do
            writeIORef stack xs'
            return (Just x)

peek :: Stack a -> IO (Maybe a)
peek stack = do
    xs <- readIORef stack
    case xs of
        [] -> return Nothing
        (x:_) -> return (Just x)
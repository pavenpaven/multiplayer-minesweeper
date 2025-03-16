module Buffer where

import Control.Monad

import Control.Concurrent
import Control.Concurrent.MVar

data Buffer a = MkBuffer (MVar [a]) (MVar ()) -- A buffer is valid iff the semaphore is empty only if the list mvar is empty. Any function manipulating this most make shure that buffers satisfy this invariant.


newBuffer :: [a] -> IO (Buffer a)
newBuffer l =
  do list <- newMVar l
     sema <- if null l then newEmptyMVar else newMVar ()
     return $ MkBuffer list sema

newEmptyBuffer :: IO (Buffer a)
newEmptyBuffer = newBuffer []

readBuffer :: Buffer a -> IO a
readBuffer (MkBuffer l sema) =
  do takeMVar sema
     contents <- takeMVar l
     unless (null $ tail contents) (tryPutMVar sema () >> return ())
     putMVar l (tail contents)     
     return (head contents)

modifyBuffer :: Buffer a -> ([a] -> [a]) -> IO ()
modifyBuffer (MkBuffer l sema) f =
  do contents <- takeMVar l
     let contents' = f contents
     putMVar l contents'
     tryTakeMVar sema
     unless (null contents') (tryPutMVar sema () >> return ())
     
     
writeBuffer :: Buffer a -> a -> IO ()
writeBuffer (MkBuffer l sema) a =
  do modifyMVar_ l (\contents -> return (contents ++ [a]))
     tryPutMVar sema ()
     return ()
     


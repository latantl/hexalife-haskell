module Names where

  import Data.IORef
  import System.IO.Unsafe

  type Ref a = IORef a
  fromI a = fromIntegral a
  new a = unsafePerformIO $ newIORef a
  set a = writeIORef $ a
  val a = readIORef a
  emptyIO :: IO ()
  emptyIO = return ()

  inInterval :: (Ord n) => n -> n -> n -> n
  inInterval n left right
    | n < left = left
    | n > right = right
    | otherwise = n

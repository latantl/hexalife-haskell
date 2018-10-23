module Names where

  import Data.IORef
  import System.IO.Unsafe

  fromI a = fromIntegral a
  new a = unsafePerformIO $ newIORef a
  set a = writeIORef $ a
  val a = readIORef a

  inInterval :: (Ord n) => n -> n -> n -> n
  inInterval n left right
    | n < left = left
    | n > right = right
    | otherwise = n

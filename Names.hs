module Names where

  import Data.IORef
  import System.IO.Unsafe

  fromI a = fromIntegral a
  new a = unsafePerformIO $ newIORef a
  set a = writeIORef $ a
  val a = readIORef a

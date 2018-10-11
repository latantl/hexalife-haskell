module Life where

  import Data.Array.IO
  import Data.IORef

  import Shapes
  import Names

  c = Circle (Vec 0 0) 0.1 (Col 0.07 0 0.2)
  c1 = Circle (Vec 1 1) 0.15 (Col 0 0.25 0.5)

  type Neighbour = IORef Cell
  data Cell = Cell Circle [Neighbour] (Bool, Bool)

  cells :: IORef [[IORef Cell]]
  cells = new [[]]

  setCellAt :: Int -> Int -> IO ()
  setCellAt i j = do
    

  drawLife :: Float -> IO ()
  drawLife accuracy = do
    drawCircle c (round accuracy)
    drawCircle c1 (round accuracy)

  updateLife :: IO ()
  updateLife = do
    return ()

  array = do
    arr <- newArray (1,10) "hy" :: IO (IOArray Int String)
    a <- readArray arr 1
    writeArray arr 2 "k"
    b <- readArray arr 2
    print (a,b)

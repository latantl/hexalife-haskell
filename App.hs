module App where

  import Life
  import Names
  import Shapes
  import Graphics.UI.GLUT
  import ScrollWindow

  main :: IO ()
  main = do
    (_progName, _args) <- getArgsAndInitialize
    setBackgrounColor $ Col 0.055 0 0.105
    maxZoom `set` 5
    minTrans `set` (Vec (-5) (-5))
    maxTrans `set` (Vec 5 5)
    onDrawFunction `set` Just onDraw
    onTickFunction `set` Just onTick
    tickPeriodLength `set` 400
    onClickFunction `set` Just onClick
    runScrollWindow "Game Of Life" (Size 640 640) (Position 0 0)

  onDraw = do
    scale <- val zoomScale
    drawLife (20 * log (scale + 1.7) + 5)

  onTick = do
    return ()

  onClick :: Vec -> IO ()
  onClick p = do
    return ()

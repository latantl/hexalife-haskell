module App where

  import Life
  import Names
  import Shapes
  import Graphics.UI.GLUT
  import ScrollWindow

  goBackground = Col 0.055 0 0.105
  pauseBackground = Col 0 0 0
  isGoing = new False

  main :: IO ()
  main = do
    initLife
    (_progName, _args) <- getArgsAndInitialize
    setBackgrounColor pauseBackground
    maxZoom `set` 5
    onDrawFunction `set` Just onDraw
    onTickFunction `set` Just onTick
    tickPeriodLength `set` 400
    onClickFunction `set` Just onClick
    onKeyboardFunction `set` Just keyboard
    initScrollWindow "Game Of Life" (Size 640 640) (Position 0 0)
    setCorners (Vec (-5) (-5)) (Vec 5 5)
    mainLoop

  keyboard :: KeyboardCallback
  keyboard char _ = do
    case char of
      '\r' -> setReset
      otherwise -> print char

  onDraw :: IO ()
  onDraw = do
    drawLife

  onTick :: IO ()
  onTick = do
    goes <- val isGoing
    if goes then updateLife else return ()

  onClick :: Vec -> IO ()
  onClick p = do
    return ()

  setReset = do
    goes <- val isGoing
    setBackgrounColor $ if goes then pauseBackground else goBackground
    isGoing `set` not goes
    postRedisplay Nothing

  adjustCorners :: IO ()
  adjustCorners = do
    return ()

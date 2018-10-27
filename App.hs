module App where

  import Data.Sequence as S

  import Life
  import Names
  import Shapes
  import Graphics.UI.GLUT
  import ScrollWindow
  import SeqExtensions

  goBackground = Col 0.055 0 0.105
  pauseBackground = Col 0 0 0
  isGoing = new False

  main :: IO ()
  main = do
    (_progName, _args) <- getArgsAndInitialize
    setBackgroundColor pauseBackground
    maxZoom `set` 5
    onDrawFunction `set` Just onDraw
    onTickFunction `set` Just onTick
    tickPeriodLength `set` 400
    onClickFunction `set` Just onClick
    onKeyboardFunction `set` Just keyboard
    initScrollWindow "Game Of Life" (Size 640 640) (Position 0 0)
    initLife
    mainLoop

  keyboard :: KeyboardCallback
  keyboard char _ = do
    case char of
      '\r' -> setReset
      otherwise -> print char

  onDraw :: IO ()
  onDraw = do
    forAllCells drawCell

  onTick :: IO ()
  onTick = do
    goes <- val isGoing
    if not goes then return () else do
      return ()

  onClick :: Vec -> IO ()
  onClick p = do
    realPos <- posToReal p
    switchCellAt realPos

  setReset = do
    goes <- val isGoing
    setBackgroundColor $ if goes then pauseBackground else goBackground
    isGoing `set` not goes
    postRedisplay Nothing

  adjustCorners = do
    crows <- val cellWorld
    (cref1 :<| _) <- val (seqHead crows)
    (_ :|> cref2) <- val (seqLast crows)
    (Cell (Circle o1 _ _) _ _) <- val cref1
    (Cell (Circle o2 _ _) _ _) <- val cref2
    setCorners o1 o2

  initLife = do
    initCellBase
    mapM_ (\_ -> pushTopRows) [0..4]
    mapM_ (\_ -> pushBottomRows) [0..4]
    mapM_ (\_ -> pushLeftColumn) [0..9]
    mapM_ (\_ -> pushRightColumn) [0..9]
    adjustCorners

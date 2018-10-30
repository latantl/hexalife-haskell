module App where

  import Data.Sequence as S hiding(splitAt)
  import Life
  import Names
  import Shapes
  import Graphics.UI.GLUT
  import ScrollWindow
  import SeqExtensions
  import LifeFunction

  goBackground = Col 0.055 0 0.105
  pauseBackground = Col 0 0 0
  isGoing = new False
  transFunc :: Ref LifeFunction
  transFunc = new $ defaultFunction

  main :: Maybe LifeFunction -> IO ()
  main trans = do
    maybe emptyIO (transFunc `set`) trans
    (_progName, _args) <- getArgsAndInitialize
    setBackgroundColor pauseBackground
    maxZoom `set` 5
    onDrawFunction `set` Just onDraw
    onTickFunction `set` Just onTick
    tickPeriodLength `set` 400
    onClickFunction `set` Just onClick
    onKeyboardFunction `set` Just keyboard
    onSpecKeyFunction `set` Just specialKey
    initScrollWindow "Game Of Life" (Size 640 640) (Position 0 0)
    initLife
    mainLoop

  keyboard :: KeyboardCallback
  keyboard char _ = case char of
      '\r' -> setReset
      'r' -> forAllCells randomizeState
      'c' -> forAllCells (\cref -> setState cref False)
      otherwise -> emptyIO

  specialKey :: SpecialCallback
  specialKey key _ = do
    let isModified = new True
    case key of
      KeyLeft -> do
        pushLeftColumn
        pushLeftColumn
      KeyRight -> do
        pushRightColumn
        pushRightColumn
      KeyDown -> pushBottomRows
      KeyUp -> pushTopRows
      otherwise -> isModified `set` False
    adjustNeeded <- val isModified
    if adjustNeeded then adjustCorners else emptyIO

  onDraw = do
    forAllCells drawCell

  onTick = do
    goes <- val isGoing
    if not goes then emptyIO else do
      forAllCells newState
      forAllCells updateState
      where
        newState cref = do
          s <- getState cref
          ns <- neighbourStates cref
          tf <- val transFunc
          cref `setNewState` (tf s ns)

  onClick pos = do
    realPos <- posToReal pos
    switchCellStateAt realPos

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

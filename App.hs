module App where

import Data.Sequence as S hiding(splitAt)
import Life
import HexaLife
--import SquareLife
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
  tickPeriodLength `set` 100
  onClickFunction `set` Just onClick
  onKeyboardFunction `set` Just keyboard
  onSpecKeyFunction `set` Just specialKey
  initScrollWindow "Game Of Life" (Size 640 640) (Position 0 0)
  initLife
  mainLoop

keyboard :: KeyboardCallback
keyboard char _ = case char of
    '\r' -> startPause
    'r' -> forAllCells randomizeState
    'c' -> forAllCells (\cref -> setState cref False)
    'l' -> toggleCropLock
    otherwise -> emptyIO

specialKey :: SpecialCallback
specialKey key _ = do
  let isModified = new True
  case key of
    KeyLeft -> pushL
    KeyRight -> pushR
    KeyDown -> pushB
    KeyUp -> pushT
    otherwise -> isModified `set` False
  adjustNeeded <- val isModified
  if adjustNeeded then adjustCorners else emptyIO

onDraw = do
  forAllCells drawCell
  --forAllCells $ drawCellConnections neighbourCount

onTick = do
  goes <- val isGoing
  if not goes then emptyIO else do
    alive <- updateLife neighbourCount transFunc pushB pushT pushL pushR
    if alive then emptyIO else pause

onClick pos = do
  realPos <- posToReal pos
  switchCellStateAt realPos

pause = do
  isGoing `set` False
  setBackgroundColor $ pauseBackground
start = do
  isGoing `set` True
  setBackgroundColor $ goBackground
startPause = do
  goes <- val isGoing
  if goes then pause else start

initLife = do
  initCellBase
  let n = minSize `div` 2 - 1
  mapM_ (\_ -> pushT) [1..n]
  mapM_ (\_ -> pushR) [1..n]
  adjustCorners

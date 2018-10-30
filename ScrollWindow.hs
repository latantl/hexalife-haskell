module ScrollWindow where

  import Graphics.UI.GLUT
  import Data.IORef
  import Data.Maybe
  import System.IO.Unsafe

  import Names
  import Shapes

  backRect = new $ rectangle (Vec (-1) (-1)) (Vec 1 1) (Col 0 0 0)
  scrollColor = new $ Col 0.8 0.8 0.8
  wheelZoom = 1.05 :: Float
  defWidth = new (640 :: Float)
  defHeight = new (640 :: Float)
  width = new (0 :: Float)
  height = new (0 :: Float)
  winScale = new $ Vec 1 2
  zoomScale = new (1 :: Float)
  statTrans = new $ Vec 0 0
  latestDown = new $ Vec 0 0
  motionTrans = new $ Vec 0 0
  mouseMoved = new False
  minTrans = new $ Vec (-1) (-1)
  maxTrans = new $ Vec 1 1
  maxZoom = new (10 :: Float)
  verScrollbar = new $ rectangle (Vec 0 0) (Vec 0 0) (Col 0 0 0)
  horScrollbar = new $ rectangle (Vec 0 0) (Vec 0 0) (Col 0 0 0)
  onDrawFunction = new (Nothing :: Maybe (IO ()))
  onTickFunction = new (Nothing :: Maybe (IO ()))
  tickPeriodLength = new (1000 :: Int)
  onClickFunction = new (Nothing :: Maybe ClickCallback)
  onKeyboardFunction = new (Nothing :: Maybe KeyboardCallback)
  onSpecKeyFunction = new (Nothing :: Maybe SpecialCallback)
  type ClickCallback = Vec -> IO ()

  initScrollWindow :: String -> Size -> Position -> IO ()
  initScrollWindow appName (Size w h) position = do
    defWidth `set` (fromI w)
    defHeight `set` (fromI h)
    width `set` (fromI w)
    height `set` (fromI h)
    initialWindowSize $= (Size w h)
    initialWindowPosition $= position
    initialDisplayCapabilities $= [With DisplaySamples]
    _window <- createWindow appName
    displayCallback $= display
    reshapeCallback $= Just reshape
    mouseCallback $= Just mouse
    motionCallback $= Just motion
    millis <- val tickPeriodLength
    func <- val onTickFunction
    maybe emptyIO (\ _ -> addTimerCallback millis timer) func
    kf <- val onKeyboardFunction
    keyboardCallback $= kf
    spkf <- val onSpecKeyFunction
    specialCallback $= spkf
    adjustScrollbars

  display :: DisplayCallback
  display = do
    clear [ ColorBuffer ]
    background <- val backRect
    drawFilledPolygon background
    (Vec sx sy) <- scaleVec
    (Vec tx ty) <- transVec
    preservingMatrix $ do
      scale sx sy (1::Float)
      translate $ Vector3 tx ty (0::Float)
      zs <- val zoomScale
      onDraw <- val onDrawFunction
      maybe emptyIO (\f -> f) onDraw
    hr <- val horScrollbar
    vr <- val verScrollbar
    drawFilledPolygon hr
    drawFilledPolygon vr
    swapBuffers

  timer :: IO ()
  timer = do
    func <- val onTickFunction
    fromJust func
    millis <- val tickPeriodLength
    addTimerCallback millis timer
    postRedisplay Nothing

  reshape :: ReshapeCallback
  reshape (Size newWidth newHeight) = do
    dw <- val defWidth
    dh <- val defHeight
    let (nw, nh) = (fromI newWidth, fromI newHeight)
    winScale `set` (Vec (dw / nw) ((dh / nh) * dw / dh))
    width `set` nw
    height `set` nh
    adjustZoom
    adjustScrollbars
    viewport $= (Position 0 0, Size newWidth newHeight)
    postRedisplay Nothing

  mouse :: MouseCallback
  mouse button state pixpos = do
    pos <- pixToPos pixpos
    case button of
      WheelUp -> do
        zoom pos wheelZoom
        adjustScrollbars
      WheelDown -> do
        zoom pos (1 / wheelZoom)
        adjustScrollbars
      LeftButton -> case state of
        Down -> do
          p <- posToReal pos
          latestDown `set` p
          mouseMoved `set` False
        Up -> do
          moved <- val mouseMoved
          if moved then do
            tv <- val statTrans
            mt <- val motionTrans
            statTrans `set` (tv `add` mt)
            motionTrans `set` (Vec 0 0)
            latestDown `set` (Vec 0 0)
            adjustTranslation
            adjustScrollbars
          else do
            end <- posToReal pos
            func <- val onClickFunction
            maybe emptyIO (\f -> f pos) func
      otherwise -> emptyIO
    postRedisplay Nothing

  motion :: MotionCallback
  motion pixpos = do
    mouseMoved `set` True
    realPos <- pixToReal pixpos
    start <- val latestDown
    motionTrans `set` (realPos `sub` start)
    adjustScrollbars
    postRedisplay Nothing

  zoom :: Vec -> Float -> IO ()
  zoom pos ammount = do
    p1 <- posToReal pos
    s <- val zoomScale
    (Vec mx my) <- minScaleVec
    let minZ = max mx my
    maxZ <- val maxZoom
    zoomScale `set` (inInterval (ammount * s) minZ maxZ)
    p2 <- posToReal pos
    tv <- val statTrans
    statTrans `set` (tv `add` p2 `sub` p1)
    adjustTranslation

  adjustZoom :: IO ()
  adjustZoom = do
    h <- val height
    w <- val width
    zoom (Vec (w / 2) (h / 2)) 1

  adjustScrollbars :: IO ()
  adjustScrollbars = do
    (Vec tx ty) <- transVec
    (Vec sx sy) <- scaleVec
    (Vec mx my) <- minScaleVec
    (Vec lx ly) <- val minTrans
    (Vec rx ry) <- val maxTrans
    w <- val width
    h <- val height
    let ox = (1 - (tx - lx) / (rx - lx)) * w
    let oy = ((ty - ly) / (ry - ly)) * h
    let ax = (mx / sx) * w / 2
    let ay = (my / sy) * h / 2
    rx0 <- posToNorm $ Vec (ox - ax) (1)
    rx1 <- posToNorm $ Vec (ox + ax) (3)
    ry0 <- posToNorm $ Vec 1 (oy - ay)
    ry1 <- posToNorm $ Vec 3 (oy + ay)
    c <- val scrollColor
    horScrollbar `set` rectangle rx0 rx1 c
    verScrollbar `set` rectangle ry0 ry1 c

  minScaleVec :: IO (Vec)
  minScaleVec = do
    (Vec sx sy) <- val winScale
    (Vec ax ay) <- val minTrans
    (Vec bx by) <- val maxTrans
    return (Vec (2 / ((bx - ax) * sx)) (2 / ((by - ay) * sy)))

  scaleVec :: IO (Vec)
  scaleVec = do
    (Vec sx sy) <- val winScale
    zs <- val zoomScale
    return (Vec (sx * zs) (sy * zs) )

  transVec :: IO (Vec)
  transVec = do
    (Vec tx ty) <- val statTrans
    (Vec mx my) <- val motionTrans
    return (Vec (tx + mx) (ty + my) )

  adjustTranslation :: IO ()
  adjustTranslation = do
    (Vec ax ay) <- val minTrans
    (Vec bx by) <- val maxTrans
    (Vec tx ty) <- val statTrans
    sv <- val winScale
    s <- val zoomScale
    let (Vec sx sy) = sv `mul` s
    let x = inInterval tx (ax + 1 / sx) (bx - 1 / sx)
    let y = inInterval ty (ay + 1 / sy) (by - 1 / sy)
    statTrans `set` (Vec x y)

  pixToReal :: Position -> IO Vec
  pixToReal pix = do
    pos <- pixToPos pix
    result <- posToReal pos
    return result

  pixToPos :: Position -> IO Vec
  pixToPos (Position x y) = return $ Vec (fromI x) (fromI y)

  posToReal :: Vec -> IO Vec
  posToReal p = do
    n <- posToNorm p
    result <- normToReal n
    return result

  posToNorm :: Vec -> IO Vec
  posToNorm (Vec px py) = do
    w <- val width
    h <- val height
    let x = px / (w / 2) - 1
    let y = 1 - py / (h / 2)
    return (Vec x y)

  normToReal :: Vec -> IO Vec
  normToReal (Vec nx ny) = do
    (Vec sx sy) <- val winScale
    (Vec tx ty) <- val statTrans
    s <- val zoomScale
    let x = nx / (sx * s) - (tx)
    let y = ny / (sy * s) - (ty)
    return (Vec x y)

  setBackgroundColor :: Col -> IO ()
  setBackgroundColor c = do
    (FilledPolygon points _) <- val backRect
    backRect `set` (FilledPolygon points c)

  setCorners :: Vec -> Vec -> IO ()
  setCorners leftLower rightUpper = do
    minTrans `set` (rightUpper `mul` (-1))
    maxTrans `set` (leftLower `mul` (-1))
    adjustZoom
    adjustScrollbars

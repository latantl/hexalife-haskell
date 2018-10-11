module ScrollWindow where

  import Graphics.UI.GLUT
  import Data.IORef
  import Data.Maybe
  import System.IO.Unsafe

  import Names
  import Shapes

  backRect = new $ Rectangle (Vec (-1) (-1)) (Vec 1 1) (Col 0 0 0)
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
  verScrollbar = new $ Rectangle (Vec 0 0) (Vec 0 0) (Col 0 0 0)
  horScrollbar = new $ Rectangle (Vec 0 0) (Vec 0 0) (Col 0 0 0)
  onDrawFunction = new (Nothing :: Maybe (IO ()))
  onTickFunction = new (Nothing :: Maybe (IO ()))
  tickPeriodLength = new (1000 :: Int)
  onClickFunction = new (Nothing :: Maybe ClickCallback)
  type ClickCallback = Vec -> IO ()

  runScrollWindow :: String -> Size -> Position -> IO ()
  runScrollWindow appName (Size w h) position = do
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
    case func of
      Nothing -> return ()
      otherwise -> addTimerCallback millis timer
    adjustScrollbars
    mainLoop

  display :: DisplayCallback
  display = do
    clear [ ColorBuffer ]
    background <- val backRect
    drawRectangle background
    (Vec sx sy) <- scaleVec
    (Vec tx ty) <- transVec
    preservingMatrix $ do
      scale sx sy (1::Float)
      translate $ Vector3 tx ty (0::Float)
      zs <- val zoomScale
      onDraw <- val onDrawFunction
      case onDraw of
        Nothing -> return ()
        otherwise -> fromJust onDraw
    hr <- val horScrollbar
    vr <- val verScrollbar
    drawRectangle hr
    drawRectangle vr
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
    adjustZoom 0 0 1
    viewport $= (Position 0 0, Size newWidth newHeight)
    postRedisplay Nothing

  mouse :: MouseCallback
  mouse button state (Position x y) = do
    case button of
      WheelUp -> do
        adjustZoom x y wheelZoom
        adjustScrollbars
      WheelDown -> do
        adjustZoom x y (1 / wheelZoom)
        adjustScrollbars
      LeftButton -> case state of
        Down -> do
          pos <- pixToReal x y
          latestDown `set` pos
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
            end <- pixToReal x y
            func <- val onClickFunction
            case func of
              Nothing -> return ()
              otherwise -> fromJust func $ Vec (fromI x) (fromI y)
      otherwise -> return ()
    postRedisplay Nothing

  motion :: MotionCallback
  motion (Position x y) = do
    mouseMoved `set` True
    pos <- pixToReal x y
    start <- val latestDown
    motionTrans `set` (pos `sub` start)
    adjustScrollbars
    postRedisplay Nothing

  adjustZoom :: GLint -> GLint -> Float -> IO ()
  adjustZoom x y ammount = do
    p1 <- pixToReal x y
    s <- val zoomScale
    (Vec mx my) <- minScaleVec
    let minZ = max mx my
    maxZ <- val maxZoom
    zoomScale `set` (inInterval (ammount * s) minZ maxZ)
    p2 <- pixToReal x y
    tv <- val statTrans
    statTrans `set` (tv `add` p2 `sub` p1)
    adjustTranslation

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
    horScrollbar `set` Rectangle rx0 rx1 c
    verScrollbar `set` Rectangle ry0 ry1 c

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

  inInterval :: (Ord n) => n -> n -> n -> n
  inInterval n left right
    | n < left = left
    | n > right = right
    | otherwise = n

  pixToReal :: GLint -> GLint -> IO Vec
  pixToReal px py = do
    result <- posToReal $ Vec (fromI px) (fromI py)
    return result

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

  setBackgrounColor :: Col -> IO ()
  setBackgrounColor c = do
    (Rectangle p0 p1 _) <- val backRect
    backRect `set` (Rectangle p0 p1 c)

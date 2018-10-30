module Life where

  import Data.Ref hiding (new)
  import Data.Maybe
  import Data.Array.IO hiding (index)
  import Data.Sequence as S
  import Graphics.UI.GLUT( PrimitiveMode( Lines ) )
  import System.Random
  import SeqExtensions
  import Shapes
  import Names

  deadColor = Col 0.07 0 0.2
  aliveColor = Col 0 0.25 0.5
  radius = 0.1 :: Float
  distInRow = 1.9 * radius
  shift = distInRow / 2
  distInCol = distInRow * 0.8660254 --sinus(60°)
  circleDots = 6 :: Int
  stepRight = Vec distInRow 0
  stepLeft = Vec ((-1) * distInRow) 0


  type Neighbours = IOArray Int (Maybe (Ref Cell))
  type CellState = (Bool, Bool)
  data Cell = Cell FilledPolygon Neighbours CellState
  createCellAt :: Vec -> IO (Ref Cell)
  createCellAt origo = do
    let c = circle origo radius deadColor circleDots
    ns <- newArray (0, 5) Nothing
    return $ new $ Cell c ns (False, False)
  type CellSeq = Seq (Ref Cell)
  type CellRow = Ref CellSeq
  type CellWorld = Ref (Seq CellRow)
  cellWorld = new $ Empty :: CellWorld

  switchCellStateAt p = do
    let (Vec x y) = p
    crows <- val cellWorld
    lowestRow <- val (seqHead crows)
    highestRow <- val (seqLast crows)
    (Cell (Circle (Vec _ lowestY) _ _) _ _) <- val (seqHead lowestRow)
    (Cell (Circle (Vec _ highestY) _ _) _ _) <- val (seqHead highestRow)
    (first : rest) <- if y < lowestY then possibleInRowAt x lowestRow
      else if y > highestY then possibleInRowAt x highestRow
        else do
          let i = floor $ (y - lowestY) / distInCol
          row1 <- val (crows `index` i)
          row2 <- val (crows `index` (i + 1))
          cells1 <- possibleInRowAt x row1
          cells2 <- possibleInRowAt x row2
          return $ cells1 ++ cells2
    cellToSwitch <- foldl (minDistanceTo p) (return first) rest
    switchCellState cellToSwitch
  possibleInRowAt x cseq = do
    let firstCell = seqHead cseq
    let lastCell = seqLast cseq
    (Cell (Circle (Vec lowestX _) _ _) _ _) <- val firstCell
    (Cell (Circle (Vec highestX _) _ _) _ _) <- val lastCell
    if x < lowestX then return [firstCell]
      else if x > highestX then return [lastCell]
        else do
          let i = floor $ (x - lowestX) / distInRow
          return [cseq `index` i, cseq `index` (i + 1)]
  minDistanceTo p iocref1 cref2 = do
    cref1 <- iocref1
    (Cell (Circle o1 _ _) _ _) <- val cref1
    (Cell (Circle o2 _ _) _ _) <- val cref2
    let (d1, d2) = (p `dist` o1, p `dist` o2)
    return $ if d1 < d2 then cref1 else cref2
  switchCellState cref = do
    oldState <- getState cref
    setState cref $ not oldState
  randomizeState cref = do
    gen <- newStdGen
    let (state, _) = random gen
    setState cref state

  getState cref = do
    (Cell _ _ (s0, _)) <- val cref
    return s0
  getNeighbourState cref index = do
    (Cell _ neighbours _) <- val cref
    neighbour <- readArray neighbours index
    maybe (return False) returnState neighbour
    where
      returnState n = do
        state <- getState n
        return state
  neighbourStates cref = do
    states <- foldl (add cref) (return []) [0..5]
    return states
    where
      add cref iolist dir = do
        list <- iolist
        s <- getNeighbourState cref dir
        return $ s:list

  setNewState cref presentState = do
    (Cell (Circle o ps _) ns (pastState, _)) <- val cref
    let newCol = if presentState then aliveColor else deadColor
    cref `set` (Cell (Circle o ps newCol) ns (pastState, presentState))
  updateState cref = do
    (Cell c ns (_, newState)) <- val cref
    cref `set` (Cell c ns (newState, newState))
  setState cref state = do
    setNewState cref state
    updateState cref

  initCellBase = do
    c1 <- createCellAt (Vec 0 0)
    c2 <- createCellAt (Vec shift distInCol)
    bindCells c1 1 c2
    let first = new $ S.fromList [c1]
    let second = new $ S.fromList [c2]
    cellWorld `set` (S.fromList [first,second])

  copyCell :: Vec -> Ref Cell -> IO (Ref Cell)
  copyCell trans cref = do
    (Cell (Circle o _ _) _ _) <- val cref
    createCellAt $ o `add` trans

  copyCellSeq :: CellSeq -> Vec -> IO (CellSeq)
  copyCellSeq cseq trans = mapM (copyCell trans) cseq

  bindCellSeq :: CellSeq -> [Int] -> IO (Ref Cell)
  bindCellSeq (first :<| rest) dirs = do
    foldl f (return first) $ zipSeqList rest dirs
    where
      f previous (actual, dir) = do
        prev <- previous
        bindCells prev dir actual
        return actual

  copyCellRow :: CellRow -> Vec -> IO (CellRow)
  copyCellRow crow trans = do
    cseq <- val crow
    cseqNew <- copyCellSeq cseq trans
    return (new cseqNew)

  bindCellRow :: CellRow -> IO (Ref Cell)
  bindCellRow crow = do
    cseq <- val crow
    bindCellSeq cseq [0,0..]

  bindCellRows :: CellRow -> Int -> Int -> CellRow -> IO ()
  bindCellRows crow1 dir1 dir2 crow2 = do
    (rest1 :|> last1) <- val crow1
    rest2 <- val crow2
    let (_ :|> last2) = rest2
    bindCells last1 dir1 last2
    foldl f (return rest2) rest1
    emptyIO
    where
      f row cell = do
        (n1 :<| n2 :<| rest) <- row
        bindCells cell dir1 n1
        bindCells cell dir2 n2
        return (n2 <| rest)

  bindCellColumns :: CellSeq -> CellSeq -> IO ()
  bindCellColumns cseq1 cseq2 = do
    let (first1 :<| tail1) = cseq1
    let (first2 :<| tail2) = cseq2
    let (rest1 :|> last1) = tail1
    let (rest2 :|> last2) = tail2
    bindCells first2 2 $ seqHead rest1
    bindCells last1 5 $ seqLast rest2
    mapM_ (\(c1, c2) -> bindCells c1 0 c2) $ S.zip cseq1 cseq2
    mapM_ (\(c1, c2) -> bind c1 c2) $ everySecond $ S.zip rest1 rest2
    where
      bind c1 c2 = do
        (Cell _ neighbours1 _) <- val c1
        (Just n1) <- readArray neighbours1 5
        (Just n2) <- readArray neighbours1 1
        bindCells c2 4 n1
        bindCells c2 2 n2

  pushRows oldRow pushRow bindRows trans1 trans2 = do
    crows <- val cellWorld
    let r = oldRow crows
    newRow1 <- copyCellRow r trans1
    newRow2 <- copyCellRow newRow1 trans2
    cellWorld `set` (pushRow newRow2 $ pushRow newRow1 crows)
    bindCellRow newRow1
    bindCellRow newRow2
    bindRows newRow1 2 1 r
    bindRows newRow1 4 5 newRow2
  pushBottomRows = do
    pushRows seqHead pushFront bindCellRows trans1 trans2
    where
      trans1 = (Vec shift ((-1) * distInCol))
      trans2 = (Vec ((-1) * shift) ((-1) * distInCol))
  pushTopRows = do
    pushRows seqLast pushBack reversalBind trans1 trans2
    where
      trans1 = (Vec ((-1) * shift) distInCol)
      trans2 = (Vec shift distInCol)
      reversalBind r1 d1 d2 r2 = bindCellRows r2 d1 d2 r1

  pushColumn oldColumn pushCell bindColumns trans = do
    oldc <- oldColumn
    newc <- copyCellSeq oldc trans
    bindCellSeq newc $ cycle [1,2]
    bindColumns newc oldc
    world <- val cellWorld
    mapM_ (\(r, c) -> pushCell r c) (S.zip world newc)
  pushLeftColumn =
    pushColumn firstColumn pushFrontRow bindCellColumns stepLeft
    where
      firstColumn = mapCellRows seqHead
      pushFrontRow crow cref = do
        cseq <- val crow
        crow `set` (cref <| cseq)
  pushRightColumn =
    pushColumn lastColumn pushBackRow reversalBind stepRight
    where
      reversalBind a b = bindCellColumns b a
      lastColumn = mapCellRows seqLast
      pushBackRow crow cref = do
        cseq <- val crow
        crow `set` (cseq |> cref)

  forAllCells :: (Ref Cell -> IO ()) -> IO ()
  forAllCells cellFunc = do
    w <- val cellWorld
    mapM_ rowFunc w
    where
      rowFunc row = do
        r <- val row
        mapM_ cellFunc r

  mapCellRows :: (CellSeq -> Ref Cell) -> IO (CellSeq)
  mapCellRows func = do
    crows <- val cellWorld
    mapM f crows
    where
      f cellRow = do
        cellSeq <- val cellRow
        return (func cellSeq)

  bindCells :: Ref Cell -> Int -> Ref Cell -> IO ()
  bindCells c1 dir c2 = do
    (Cell _ neighbours1 _) <- val c1
    (Cell _ neighbours2 _) <- val c2
    writeArray neighbours1 dir $ Just c2
    writeArray neighbours2 ((dir + 3) `mod` 6)$ Just c1

  drawCell :: Ref Cell -> IO ()
  drawCell cell = do
    (Cell circle neighbours _) <- val cell
    drawFilledPolygon circle

  drawCellConnections :: Ref Cell -> IO ()
  drawCellConnections cell = mapM_ (tryDrawConn cell) [0..5]

  tryDrawConn :: Ref Cell -> Int -> IO ()
  tryDrawConn cell dir = do
    (Cell _ neighbours _) <- val cell
    n <- readArray neighbours dir
    maybe emptyIO (drawConn cell dir) n
    where
      drawConn cell dir neighbour = do
        (Cell (Circle o2 _ _) _ _) <- val neighbour
        (Cell (Circle o1 _ _) _ _) <- val cell
        let v1 = o1 `add` (angleToVec (Vec 0 0) (0.1 * radius) ((fromI dir) * pi / 3))
        let v2 = o1 `add` ((o2 `sub` o1) `mul` 0.3)
        drawPrimitive Lines [v1, v2] (Col 1 1 1)

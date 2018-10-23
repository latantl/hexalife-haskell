module Life where

  import Data.IORef
  import Data.Maybe
  import Data.Map as M hiding (foldl)
  import Data.Sequence as S
  import Graphics.UI.GLUT( PrimitiveMode( Lines ) )

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

  type NeighbourMap = IORef (Map Int (IORef Cell))
  type CellState = (Bool, Bool)
  data Cell = Cell FilledPolygon NeighbourMap CellState
  createCellAt :: Vec -> IORef Cell
  createCellAt origo = let
    c = circle origo radius deadColor circleDots
    in new $ Cell c (new $ M.fromList []) (False, False)
  type CellSeq = Seq (IORef Cell)
  type CellRow = IORef CellSeq
  type CellWorld = IORef (Seq CellRow)
  cellWorld = new $ Empty :: CellWorld

  drawLife = do
    forAllCells drawCell
    --forAllCells drawCellConnections

  updateLife :: IO ()
  updateLife = do
    return ()

  switchCellAt p = do
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
    cellToSwitch <- foldl (maxDistanceTo p) (return first) rest
    switchCell cellToSwitch
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
  maxDistanceTo p iocref1 cref2 = do
    cref1 <- iocref1
    (Cell (Circle o1 _ _) _ _) <- val cref1
    (Cell (Circle o2 _ _) _ _) <- val cref2
    let (d1, d2) = (p `dist` o1, p `dist` o2)
    return $ if d1 < d2 then cref1 else cref2
  switchCell cref = do
    (Cell (Circle o ps _) ns (present, _)) <- val cref
    let newState = not present
    let newColor = if newState then aliveColor else deadColor
    cref `set` (Cell (Circle o ps newColor) ns (newState, newState))

  initCellBase = do
    let c1 = createCellAt (Vec 0 0)
    let c2 = createCellAt (Vec shift distInCol)
    bindCells c1 1 c2
    let first = new $ S.fromList [c1]
    let second = new $ S.fromList [c2]
    cellWorld `set` (S.fromList [first,second])

  copyCell :: Vec -> IORef Cell -> IO (IORef Cell)
  copyCell trans cref = do
    (Cell (Circle o _ _) _ _) <- val cref
    return (createCellAt $ o `add` trans)

  copyCellSeq :: CellSeq -> Vec -> IO (CellSeq)
  copyCellSeq cseq trans = mapM (copyCell trans) cseq

  bindCellSeq :: CellSeq -> [Int] -> IO (IORef Cell)
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

  bindCellRow :: CellRow -> IO (IORef Cell)
  bindCellRow crow = do
    cseq <- val crow
    bindCellSeq cseq [0,0..]

  bindCellRows :: CellRow -> Int -> Int -> CellRow -> IO (CellSeq)
  bindCellRows crow1 dir1 dir2 crow2 = do
    (rest1 :|> last1) <- val crow1
    rest2 <- val crow2
    let (_ :|> last2) = rest2
    bindCells last1 dir1 last2
    foldl f (return rest2) rest1
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
        (Cell _ nref1 _) <- val c1
        neighbours1 <- val nref1
        bindCells c2 4 $ fromJust $ M.lookup 5 neighbours1
        bindCells c2 2 $ fromJust $ M.lookup 1 neighbours1

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

  forAllCells :: (IORef Cell -> IO ()) -> IO ()
  forAllCells cellFunc = do
    w <- val cellWorld
    mapM_ rowFunc w
    where
      rowFunc row = do
        r <- val row
        mapM_ cellFunc r

  mapCellRows :: (CellSeq -> IORef Cell) -> IO (CellSeq)
  mapCellRows func = do
    crows <- val cellWorld
    mapM f crows
    where
      f cellRow = do
        cellSeq <- val cellRow
        return (func cellSeq)

  bindCells :: IORef Cell -> Int -> IORef Cell -> IO ()
  bindCells c1 dir c2 = do
    (Cell _ neighbours1 _) <- val c1
    (Cell _ neighbours2 _) <- val c2
    n1 <- val neighbours1
    n2 <- val neighbours2
    neighbours1 `set` (insert dir c2 n1)
    neighbours2 `set` (insert ((dir + 3) `mod` 6) c1 n2)

  drawCell :: IORef Cell -> IO ()
  drawCell cell = do
    (Cell circle neighbours _) <- val cell
    drawFilledPolygon circle

  drawCellConnections :: IORef Cell -> IO ()
  drawCellConnections cell = do
    (Cell (Circle o1 _ _) neighbours _) <- val cell
    ns <- val neighbours
    mapM_ (tryDrawConn o1 ns) [0..5]
    where
      tryDrawConn o1 neighbours dir =
        drawConn o1 dir (M.lookup dir neighbours)
        where
          drawConn _ _ Nothing = return ()
          drawConn o1 dir (Just neighbour) = do
            (Cell (Circle o2 _ _) _ _) <- val neighbour
            let v1 = o1 `add` (angleToVec (Vec 0 0) (0.1 * radius) ((fromI dir) * pi / 3))
            let v2 = o1 `add` ((o2 `sub` o1) `mul` 0.3)
            drawPrimitive Lines [v1, v2] (Col 1 1 1)

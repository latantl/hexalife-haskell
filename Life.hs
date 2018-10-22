module Life where

  import Data.IORef
  import Data.Map as M hiding (foldl)
  import Data.Sequence as S
  import Graphics.UI.GLUT( PrimitiveMode( Lines ) )

  import Shapes
  import Names
  import Numeric

  deadColor = Col 0.07 0 0.2
  aliveColor = Col 0 0.25 0.5
  radius = 0.1 :: Float
  distInRow = 2.1 * radius
  shift = distInRow / 2
  distInCol = distInRow * 0.8660254 --sinus(60°)
  circleDots = 40 :: Int

  type CellRow = IORef (Seq (IORef Cell))
  cells = new $ S.fromList [] :: IORef (Seq CellRow)

  initLife = do
    initCellBase
    mapM_ (\_ -> addTopRows) [0..9]
    mapM_ (\_ -> addBottomRows) [0..9]

  drawLife = do
    forAllCells drawCell
    forAllCells drawConnections

  updateLife :: IO ()
  updateLife = do
    return ()

  type Neighbour = IORef Cell
  data Cell = Cell FilledPolygon (IORef (Map Int Neighbour)) (Bool, Bool)
  createCellAt :: Vec -> IORef Cell
  createCellAt origo = let
    c = circle origo radius deadColor circleDots
    in new $ Cell c (new $ M.fromList []) (False, False)

  initCellBase :: IO ()
  initCellBase = do
    let c1 = createCellAt (Vec 0 0)
    let c2 = createCellAt (Vec distInRow 0)
    let c3 = createCellAt (Vec shift distInCol)
    let c4 = createCellAt (Vec (distInRow + shift) distInCol)
    bindCells c1 0 c2
    bindCells c1 1 c3
    bindCells c2 2 c3
    bindCells c4 4 c2
    bindCells c4 3 c3
    let first = new $ S.fromList [c1, c2]
    let second = new $ S.fromList [c3, c4]
    cells `set` (S.fromList [first,second])

  copyCell :: Vec -> IORef Cell -> IO (IORef Cell)
  copyCell trans cellref = do
    (Cell (Circle o _ _) _ _) <- val cellref
    return (createCellAt $ o `add` trans)

  copyInRow :: CellRow -> Vec -> IORef Cell -> IO ()
  copyInRow row trans cellref = do
    r <- val row
    newCell <- copyCell trans cellref
    row `set` (r |> newCell)

  copyRow :: CellRow -> Vec -> IO (CellRow)
  copyRow rowref origoTrans = do
    oldRow <- val rowref
    let newRow = new $ S.fromList []
    mapM_ (copyInRow newRow origoTrans) oldRow
    return newRow

  bindRow :: CellRow -> IO (IORef Cell)
  bindRow crow = do
    (first :<| rest) <- val crow
    foldl f (return first) rest
    where
      f previous actual = do
        prev <- previous
        bindCells prev 0 actual
        return actual

  bindRows :: CellRow -> Int -> Int -> CellRow -> IO (Seq (IORef Cell))
  bindRows row1 dir1 dir2 row2 = do
    (r1 :|> l1) <- val row1
    r2 <- val row2
    let (_ :|> l2) = r2
    bindCells l1 dir1 l2
    foldl f (return r2) r1
    where
      f row cell = do
        (n1 :<| n2 :<| rest) <- row
        bindCells cell dir1 n1
        bindCells cell dir2 n2
        return (n2 <| rest)

  addBottomRows = do
    rows <- val cells
    let (r :<| _) = rows
    newRow1 <- copyRow r (Vec shift ((-1) * distInCol))
    newRow2 <- copyRow newRow1 (Vec ((-1) * shift) ((-1) * distInCol))
    cells `set` (newRow2 <| newRow1 <| rows)
    bindRow newRow1
    bindRow newRow2
    bindRows newRow1 2 1 r
    bindRows newRow1 4 5 newRow2

  addTopRows = do
    rows <- val cells
    let (_ :|> r) = rows
    newRow1 <- copyRow r (Vec ((-1) * shift) distInCol)
    newRow2 <- copyRow newRow1 (Vec shift distInCol)
    cells `set` (rows |> newRow1 |> newRow2)
    bindRow newRow1
    bindRow newRow2
    bindRows r 2 1 newRow1
    bindRows newRow2 4 5 newRow1

  addLeftColumn :: IO ()
  addLeftColumn = do
    return ()



  forAllCells :: (IORef Cell -> IO ()) -> IO ()
  forAllCells cellFunc = do
    w <- val cells
    mapM_ rowFunc w
    where
      rowFunc row = do
        r <- val row
        mapM_ cellFunc r

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
    drawConnections cell

  drawConnections :: IORef Cell -> IO ()
  drawConnections cell = do
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
            let v1 = o1 `add` (angleToVec (Vec 0 0) (0.1 * radius) ((fromIntegral dir) * pi / 3))
            let v2 = o1 `add` ((o2 `sub` o1) `mul` 0.3)
            drawPrimitive Lines [v1, v2] (Col 1 1 1)

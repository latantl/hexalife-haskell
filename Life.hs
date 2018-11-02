module Life where

import Data.Ref hiding (new)
import Data.Maybe
import Data.Array.IO hiding (index)
import Data.Sequence as S
import Prelude as P
import Graphics.UI.GLUT( PrimitiveMode( Lines ) )
import ScrollWindow
import System.Random
import SeqExtensions as E
import Shapes
import Names

deadColor = Col 0.07 0 0.2
aliveColor = Col 0 0.25 0.5

type Neighbours = IOArray Int (Maybe (Ref Cell))
type CellState = (Bool, Bool)
data Cell = Cell Poly Neighbours CellState
type CellSeq = Seq (Ref Cell)
type CellRow = Ref CellSeq
type CellWorld = Ref (Seq CellRow)
cellWorld = new $ Empty :: CellWorld
minSize = 10
maxSize = 200
isCropLocked = new False

getRowCount = do
  crows <- val cellWorld
  return $ S.length crows
getColumnCount = do
  crows <- val cellWorld
  cseq0 <- val (seqHead crows)
  return $ S.length cseq0
getCounts = do
  rc <- getRowCount
  cc <- getColumnCount
  return (rc, cc)

randomizeState cref = do
  gen <- newStdGen
  let (state, _) = random gen
  setState cref state
switchCellState cref = do
  (oldState, _) <- getState cref
  setState cref $ not oldState

getState cref = do
  (Cell _ _ s) <- val cref
  return s
getNeighbour cref index = do
  (Cell _ neighbours _) <- val cref
  neighbour <- readArray neighbours index
  return neighbour
getNeighbourState cref index = do
  neighbour <- getNeighbour cref index
  maybe (return False) returnState neighbour
  where
    returnState n = do
      (state, _) <- getState n
      return state
neighbourStates ncount cref = do
  states <- foldl (add cref) (return []) [0..ncount - 1]
  return states
  where
    add cref iolist dir = do
      list <- iolist
      s <- getNeighbourState cref dir
      return $ s:list
getPosition cref = do
  (Cell (Poly p _ _) _ _) <- val cref
  return p
setNewState cref presentState = do
  (Cell (Poly o ps _) ns (pastState, _)) <- val cref
  let newCol = if presentState then aliveColor else deadColor
  cref `set` (Cell (Poly o ps newCol) ns (pastState, presentState))
updateState cref = do
  (Cell c ns (_, newState)) <- val cref
  cref `set` (Cell c ns (newState, newState))
setState cref state = do
  setNewState cref state
  updateState cref
copyNeighbours bind (cref0, dirs0) (cref1, dirs1) = do
  mapM_ copy $ P.zip dirs0 dirs1
  where
    copy (d0, d1) = do
      n <- getNeighbour cref0 d0
      maybe emptyIO (bind cref1 d1) n
copySeqNeighbours bind ds0 ds1 cseqPairs =
  mapM_ (\(s0,s1) -> copyNeighbours bind (s0,ds0) (s1,ds1)) cseqPairs
bindCells ncount c1 dir c2 = do
  (Cell _ neighbours1 _) <- val c1
  (Cell _ neighbours2 _) <- val c2
  writeArray neighbours1 dir $ Just c2
  let dir2 = (dir + (ncount `div` 2)) `mod` ncount
  writeArray neighbours2 dir2 $ Just c1
bindCellSeq bind dirs (first :<| rest) = do
  foldl f (return first) $ zipSeqList rest dirs
  emptyIO
  where
    f previous (actual, dir) = do
      prev <- previous
      bind prev dir actual
      return actual
bindCellSeqs bind dir (cseq0, cseq1) =
  mapM_ (\(c0,c1) -> bind c0 dir c1) $ S.zip cseq0 cseq1

copyCell create trans cref = do
  o <- getPosition cref
  create $ o `add` trans
copySeq create cseq trans = mapM (copyCell create trans) cseq
copyRow create crow trans = do
  cseq <- val crow
  cseqNew <- copySeq create cseq trans
  return (new cseqNew)

pushRows cop1 cop2 up down bind create mode getOld pushRow trans1 trans2 = do
  crows <- val cellWorld
  let oldRow = getOld crows
  oldSeq <- val oldRow
  newRow1 <- copyRow create oldRow trans1
  newRow2 <- copyRow create newRow1 trans2
  newSeq1 <- val newRow1
  newSeq2 <- val newRow2
  bindCellSeq bind [0,0..] newSeq1
  bindCellSeq bind [0,0..] newSeq2
  bindCellSeqs bind up $ mode (newSeq1, oldSeq)
  bindCellSeqs bind down $ mode (newSeq1, newSeq2)
  cop1 $ E.zip $ mode (oldSeq, newSeq1)
  cop2 $ E.zip $ mode (newSeq2, newSeq1)
  cellWorld `set` (pushRow newRow2 $ pushRow newRow1 crows)
pushColumn create bind bindColumn copy mode getOldCell pushCell trans = double f
  where
    f = do
      oldc <- mapCellRows getOldCell
      newc <- copySeq create oldc trans
      bindColumn newc
      bindCellSeqs bind 0 $ mode (newc, oldc)
      copy $ mode (oldc, newc)
      world <- val cellWorld
      mapM_ (\(r, c) -> push r c) (S.zip world newc)
      where
        push crow cref = do
          cseq <- val crow
          crow `set` (cref `pushCell` cseq)

dropRows crop = do
  crows <- val cellWorld
  cellWorld `set` (crop crows)
dropBottom = dropRows seqTail
dropTop = dropRows seqInit
dropColumns crop = do
  crows <- val cellWorld
  mapM_ f crows
  where
    f crow =do
      cref <- val crow
      crow `set` (crop cref)
dropLeft = dropColumns seqTail
dropRight = dropColumns seqInit
crop getCount dropMin dropMax mini maxi = do
  c <- getCount
  mapM_ (\_->dropMax) [1.. (c - (max maxi minSize) - 1) `div` 2]
  mapM_ (\_->dropMin) [1.. ((max (c - minSize) mini) - 1) `div` 2]

cropRows = crop getRowCount dropBottom dropTop
cropColumns = crop getColumnCount dropLeft dropRight

forAllCells cellFunc = do
  w <- val cellWorld
  mapM_ rowFunc w
  where
    rowFunc row = do
      r <- val row
      mapM_ cellFunc r
forInnerCells cellFunc = do
  crows <- val cellWorld
  mapM_ rowFunc $ seqInter crows
  where
    rowFunc crow = do
      cseq <- val crow
      mapM_ cellFunc $ seqInter cseq

mapCellRows :: (CellSeq -> Ref Cell) -> IO (CellSeq)
mapCellRows func = do
  crows <- val cellWorld
  mapM f crows
  where
    f cellRow = do
      cellSeq <- val cellRow
      return (func cellSeq)
drawCell cell = do
  (Cell fp neighbours _) <- val cell
  drawPoly fp
drawCellConnections ncount cref = let
  nc = fromI ncount
  in mapM_ (tryDrawConn nc cref) [0..(ncount - 1)]
tryDrawConn ncount cref dir = do
  (Cell _ neighbours _) <- val cref
  n <- readArray neighbours dir
  maybe emptyIO (drawConn cref dir) n
  where
    drawConn cref dir neighbour = do
      o2 <- getPosition neighbour
      o1 <- getPosition cref
      let v1 = o1 `add` (angleToVec (Vec 0 0) (0.01) ((fromI dir) * 2 * pi / ncount))
      let v2 = o1 `add` ((o2 `sub` o1) `mul` 0.3)
      drawPrimitive Lines [v1, v2] (Col 1 1 1)

adjustCorners = do
  (p1, p2) <- getCornerPositions
  setCorners (p1 `sub` padVec) (p2 `add` padVec)
  where padVec = (Vec 0.2 0.2)
getCornerPositions = do
  crows <- val cellWorld
  (cref1 :<| _) <- val (seqHead crows)
  (_ :|> cref2) <- val (seqLast crows)
  p1 <- getPosition cref1
  p2 <- getPosition cref2
  return (p1, p2)

updateLife ncount transFunc pushB pushT pushL pushR = do
  args <- update
  let getAlive = hasAlive args
  alive <- getAlive
  if not alive then emptyIO else do
    locked <- val isCropLocked
    if locked then emptyIO else crop args
    extend args
  adjustCorners
  forAllCells updateState
  return alive
  where
    hasAlive (mini, maxi, minj, maxj) = do
      (rc, cc) <- getCounts
      return (mini<rc && maxi>=0 && minj<cc && maxj>=0)
    crop (mini, maxi, minj, maxj) = do
      cropRows mini maxi
      cropColumns minj maxj
    extend (mini, maxi, minj, maxj) = do
      (rc, cc) <- getCounts
      if mini == 0 then pushB else emptyIO
      if maxi == (rc - 1) then pushT else emptyIO
      if minj == 0 then pushL else emptyIO
      if maxj == (cc - 1) then pushR else emptyIO
    update = do
      crows <- val cellWorld
      (rc, cc) <- getCounts
      (_,mini,maxi,minj,maxj) <- foldl rowFunc (return (0,rc,-1,cc,-1)) crows
      return (mini, maxi, minj, maxj)
      where
        rowFunc args crow = do
          (rc, cc) <- getCounts
          cseq <- val crow
          (i, mini0, maxi0, minj0, maxj0) <- args
          (_, minj, maxj) <- foldl cellFunc (return (0, cc, -1)) cseq
          let updated = minj<cc && maxj>=0
          let mini = if updated then min mini0 i else mini0
          let maxi = if updated then max maxi0 i else maxi0
          return (i+1, mini, maxi, min minj0 minj, max maxj0 maxj)
          where
            cellFunc args cref = do
              (j, minj0, maxj0) <- args
              updated <- newState cref
              let minj = if updated then min minj0 j else minj0
              let maxj = if updated then max maxj0 j else maxj0
              return (j+1, minj, maxj)
              where
                newState cref = do
                  (s, _) <- getState cref
                  ns <- neighbourStates ncount cref
                  tf <- val transFunc
                  let state = tf s ns
                  cref `setNewState` state
                  return state

toggleCropLock = do
  locked <- val isCropLocked
  isCropLocked `set` (not locked)

module HexaLife
( switchCellStateAt
, initCellBase
, pushL
, pushR
, pushB
, pushT
, neighbourCount
) where

import Life
import Shapes
import SeqExtensions as E
import Names
import Data.Sequence as S
import Data.Array.IO hiding (index)

neighbourCount = 6 :: Int
radius = 0.1 :: Float
distInRow = 1.9 * radius
shift = distInRow / 2
distInCol = distInRow * 0.8660254 --sinus(60°)
circleDots = 6 :: Int

create :: Vec -> IO (Ref Cell)
create origo = do
  let c = circle origo radius deadColor circleDots
  ns <- newArray (0, neighbourCount-1) Nothing
  return $ new $ Cell c ns (False, False)

initCellBase = do
  c11 <- create (Vec 0 0)
  c21 <- create (Vec shift distInCol)
  c12 <- create (Vec distInRow 0)
  c22 <- create (Vec (shift + distInRow) distInCol)
  bind c11 1 c21
  bind c11 0 c12
  bind c12 1 c22
  bind c21 0 c22
  bind c21 5 c12
  let first = new $ S.fromList [c11, c12]
  let second = new $ S.fromList [c21, c22]
  cellWorld `set` (S.fromList [first,second])

switchCellStateAt p = do
  let (Vec x y) = p
  crows <- val cellWorld
  lowestRow <- val (seqHead crows)
  highestRow <- val (seqLast crows)
  (Vec _ lowestY) <- getPosition (seqHead lowestRow)
  (Vec _ highestY) <- getPosition (seqHead highestRow)
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
  (Vec lowestX _) <- getPosition firstCell
  (Vec highestX _) <- getPosition lastCell
  if x < lowestX then return [firstCell]
    else if x > highestX then return [lastCell]
      else do
        let i = floor $ (x - lowestX) / distInRow
        return [cseq `index` i, cseq `index` (i + 1)]
minDistanceTo p iocref1 cref2 = do
  cref1 <- iocref1
  o1 <- getPosition cref1
  o2 <- getPosition cref2
  let (d1, d2) = (p `dist` o1, p `dist` o2)
  return $ if d1 < d2 then cref1 else cref2

bind = bindCells neighbourCount
bindSeqs = bindCellSeqs bind
bindSeq = bindCellSeq bind
pushRo = pushRows cop1 cop2 2 4 bind create
  where
    cop1 = copySeqNeighbours bind [0] [1]
    cop2 = copySeqNeighbours bind [0] [5]
pushC = pushColumn create bind (bindSeq $ cycle [1,2]) bCols
  where bCols p = copySeqNeighbours bind [4,2] [5,1] $ everySecond $ E.zip p

pushB = pushRo normal seqHead pushFront trans1 trans2
  where
    trans1 = (Vec shift ((-1) * distInCol))
    trans2 = (Vec ((-1) * shift) ((-1) * distInCol))
pushT = pushRo reversed seqLast pushBack trans1 trans2
  where
    trans1 = (Vec ((-1) * shift) distInCol)
    trans2 = (Vec shift distInCol)
pushL = pushC normal seqHead pushFront trans
  where trans = Vec ((-1) * distInRow) 0
pushR = pushC reversed seqLast pushBack trans
  where trans = Vec distInRow 0

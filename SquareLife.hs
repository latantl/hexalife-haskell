module SquareLife
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

neighbourCount = 8 :: Int
circleDots = 6 :: Int
sideLength = 0.2
pad = 0.1 * sideLength
cellDistance = 2 * pad + sideLength


create :: Vec -> IO (Ref Cell)
create p = do
  let s = square p sideLength deadColor
  ns <- newArray (0, neighbourCount-1) Nothing
  return $ new $ Cell s ns (False, False)
  where a = sideLength / 2

switchCellStateAt (Vec px py) = do
  crows <- val cellWorld
  row0 <- val (seqHead crows)
  (Vec x y) <- getPosition (seqHead row0)
  let (x0, y0) = (x - pad, y - pad)
  let i = inInterval (round $ (py - y) / cellDistance) 0 (S.length crows)
  let j = inInterval (round $ (px - x) / cellDistance) 0 (S.length row0)
  row <- val (crows `index` i)
  switchCellState $ row `index` j

initCellBase = do
  c11 <- create (Vec 0 0)
  c21 <- create (Vec 0 cellDistance)
  c12 <- create (Vec cellDistance 0)
  c22 <- create (Vec cellDistance cellDistance)
  bind c11 2 c21
  bind c12 2 c22
  bind c11 0 c12
  bind c21 0 c22
  bind c11 1 c22
  bind c12 3 c21
  let first = new $ S.fromList [c11, c12]
  let second = new $ S.fromList [c21, c22]
  cellWorld `set` (S.fromList [first,second])

bind = bindCells neighbourCount
bindSeqs = bindCellSeqs bind
bindSeq = bindCellSeq bind
pushRo = pushRows cop1 cop2 2 6 bind create
  where
    cop1 = copySeqNeighbours bind [0,4] [1,3]
    cop2 = copySeqNeighbours bind [0,4] [7,5]
pushCo = pushColumn create bind (bindSeq [2,2..]) bCols
  where bCols p = copySeqNeighbours bind [2,6] [1,7] $ E.zip p

pushB = pushRo normal seqHead pushFront trans trans
  where trans = Vec 0 ((-1) * cellDistance)
pushT = pushRo reversed seqLast pushBack trans trans
  where trans = Vec 0 cellDistance
pushL = pushCo normal seqHead pushFront trans
  where trans = Vec ((-1) * cellDistance) 0
pushR = pushCo reversed seqLast pushBack trans
  where trans = Vec cellDistance 0

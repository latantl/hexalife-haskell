module LifeFunction where

  type LifeFunction = Bool -> [Bool] -> Bool

  defaultFunction :: LifeFunction
  defaultFunction = intervalFunction 3 3 2 4 regularCount

  intervalFunction bl br sl sr counter s ns =
    if s then sl <= count && count <= sr
    else bl <= count && count <= br
    where count = counter ns

  toNums ns = map (\s -> if s then 1 else 0) ns

  regularCount ns = sum $ toNums ns

  weightedCount weights ns =
    (sum $ map (\(w, s) -> w * s) $ zip weights $ toNums ns) / (sum weights)

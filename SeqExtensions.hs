module SeqExtensions where

  import Data.Sequence as S

  seqHead (result :<| _) = result
  seqLast (_ :|> result) = result

  pushFront :: a -> Seq a -> Seq a
  pushFront x xs = x <| xs

  pushBack :: a -> Seq a -> Seq a
  pushBack x xs = xs |> x

  zipSeqList :: Seq a -> [b] -> Seq (a, b)
  zipSeqList Empty _ = Empty
  zipSeqList _ [] = Empty
  zipSeqList (a :<| as) (b : bs) = (a, b) <| zipSeqList as bs

  everySecond :: Seq a -> Seq a
  everySecond Empty = Empty
  everySecond (x :<| Empty) = Empty
  everySecond (x :<| y :<| rest) = y <| (everySecond rest)

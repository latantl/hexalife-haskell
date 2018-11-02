module SeqExtensions where

import Data.Sequence as S

seqHead (result :<| _) = result
seqLast (_ :|> result) = result
seqTail (_ :<| result) = result
seqInit (result :|> _) = result
seqInter = seqTail . seqInit

pushFront x xs = x <| xs
pushBack x xs = xs |> x

zipSeqList :: Seq a -> [b] -> Seq (a, b)
zipSeqList Empty _ = Empty
zipSeqList _ [] = Empty
zipSeqList (a :<| as) (b : bs) = (a, b) <| zipSeqList as bs

everySecond :: Seq a -> Seq a
everySecond Empty = Empty
everySecond (x :<| Empty) = Empty
everySecond (x :<| y :<| rest) = y <| (everySecond rest)

zip (a, b) = S.zip a b

module WeightBiasedLeftistHeap where

import Test.HUnit

data Heap a = Empty | Bin Int a (Heap a) (Heap a)

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Empty h = h
merge h Empty = h
merge x@(Bin xWeight xMin xLeft xRight) y@(Bin yWeight yMin yLeft yRight) = if (xMin <= yMin) then (makeBin xMin xLeft (merge xRight y)) else (makeBin yMin yLeft (merge x yRight))

weight Empty = 0
weight (Bin w e left right) = w

makeBin minE x y = if (weight x) >= (weight y) then (Bin totalWeight minE x y) else (Bin totalWeight minE y x)
  where totalWeight = (weight x) + (weight y) + 1

singleton e = Bin 1 e Empty Empty

insert e h = merge (singleton e) h
findMin (Bin _ minE _ _) = minE
deleteMin (Bin _ _ x y) = merge x y

(|>) h e = insert e h

main = runTestTT $ test $ [
  "findMin" ~: 3 ~=? findMin (Empty |> 5 |> 3 |> 7),
  "deleteMin" ~: 5 ~=? findMin (deleteMin (Empty |> 5 |> 3 |> 7))
  ]

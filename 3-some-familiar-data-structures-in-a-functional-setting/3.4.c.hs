import Test.HUnit

data Heap a = Empty | Bin Int a (Heap a) (Heap a)

weight Empty = 0
weight (Bin w e left right) = w

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Empty h = h
merge h Empty = h
merge x@(Bin xWeight xMin xLeft xRight) y@(Bin yWeight yMin yLeft yRight) = if (xMin <= yMin) then (makeBin xMin xLeft (merge xRight y)) else (makeBin yMin yLeft (merge x yRight))

makeBin minE x y = if (weight x) >= (weight y) then (Bin totalWeight minE x y) else (Bin totalWeight minE y x)
        where totalWeight  = (weight x) + (weight y) + 1

findMin (Bin rank minE _ _) = minE

insert :: Ord a => a -> Heap a -> Heap a
insert e Empty = Bin 1 e Empty Empty
insert e x@(Bin w hMin left right) = if (e < hMin) then (Bin (w + 1) e x Empty) else (makeBin hMin left (insert e right))

(|>) h e = insert e h

main = runTestTT $ test $ [
  "findMin" ~: 3 ~=? findMin (Empty |> 5 |> 3 |> 7)
  ]

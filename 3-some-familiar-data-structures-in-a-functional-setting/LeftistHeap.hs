import Test.HUnit

data Heap a = Empty | Bin Int a (Heap a) (Heap a)

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Empty h = h
merge h Empty = h
merge x@(Bin xRank xMin xLeft xRight) y@(Bin yRank yMin yLeft yRight) = if (xMin <= yMin) then (makeBin xMin xLeft (merge xRight y)) else (makeBin yMin yLeft (merge x yRight))

rank Empty = 0
rank (Bin r e left right) = r

makeBin minE x y = if (rank x) >= (rank y) then (Bin ((rank y) + 1) minE x y) else (Bin ((rank x) + 1) minE y x)

singleton e = Bin 1 e Empty Empty

insert e h = merge (singleton e) h
findMin (Bin rank minE _ _) = minE
deleteMin (Bin _ _ x y) = merge x y

(|>) h e = insert e h

main = runTestTT $ test $ [
  "findMin" ~: 3 ~=? findMin (Empty |> 5 |> 3 |> 7),
  "deleteMin" ~: 5 ~=? findMin (deleteMin (Empty |> 5 |> 3 |> 7))
  ]

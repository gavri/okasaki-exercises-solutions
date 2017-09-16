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

findMin (Bin rank minE _ _) = minE

fromList :: Ord a => [a] -> Heap a
fromList l = mergeAll $ map singleton l

mergeAll :: Ord a => [Heap a] -> Heap a
mergeAll [] = Empty
mergeAll x = head $ until (\l -> length l == 1) mergeAllOnce x

mergeAllOnce :: Ord a => [Heap a] -> [Heap a]
mergeAllOnce [] = []
mergeAllOnce (a: []) = [a]
mergeAllOnce (a: b: rest) = (merge a b): (mergeAllOnce rest)

main = runTestTT $ test $ [
  "mergeAll" ~: -7 ~=? findMin (fromList [8, 2, 1, 10, -7, 0, -2])
  ]

import Test.HUnit

data Heap a = Empty | Bin Int a (Heap a) (Heap a)

rank Empty = 0
rank (Bin r e left right) = r

makeBin minE x y = if (rank x) >= (rank y) then (Bin ((rank y) + 1) minE x y) else (Bin ((rank x) + 1) minE x y)

findMin (Bin rank minE _ _) = minE

insert :: Ord a => a -> Heap a -> Heap a
insert e Empty = Bin 1 e Empty Empty
insert e x@(Bin r hMin left right) = if (e < hMin) then (Bin (r + 1) e x Empty) else (makeBin hMin left (insert e right))

(|>) h e = insert e h

main = runTestTT $ test $ [
  "findMin" ~: 3 ~=? findMin (Empty |> 5 |> 3 |> 7)
  ]

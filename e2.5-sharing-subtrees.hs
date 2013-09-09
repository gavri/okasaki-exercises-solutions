data Tree a = Empty | Bin a (Tree a) (Tree a) deriving Show
completeWithNoSharing x 0 = Empty
completeWithNoSharing x d = Bin x (completeWithNoSharing x (d - 1)) (completeWithNoSharing x (d - 1))

complete 0 x = Empty

complete d x = Bin x child child
        where child = complete (d - 1) x

split n = (ceiling half, floor half)
        where half = (fromIntegral n) / 2

incomplete 1 x = Bin x Empty Empty
incomplete 0 x = Empty
incomplete n x | even numberOfChildrenLeft = Bin x evenChild evenChild
               | otherwise = Bin x (incomplete first x) (incomplete second x)
                    where numberOfChildrenLeft = n - 1
                          (first, second) = split numberOfChildrenLeft
                          evenChild = (incomplete (div numberOfChildrenLeft 2) x)

tree x 0 = Empty
tree x 1 = Bin x Empty Empty
tree x 2 = Bin x (Bin x Empty Empty) Empty
tree x 3 = Bin x (Bin x Empty Empty) (Bin x Empty Empty)
tree x n = Bin x ((trees x) !! n1) ((trees x) !! n2)
           where (n1, n2) = split (n - 1)

trees x = map (tree x) [0..]

main = do
        print $ tree 5 3
        print $ (trees 5) !! 5

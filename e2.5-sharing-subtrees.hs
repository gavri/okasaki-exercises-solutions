data Tree a = Empty | Bin a (Tree a) (Tree a) deriving Show
completeWithNoSharing x 0 = Empty
completeWithNoSharing x d = Bin x (completeWithNoSharing x (d - 1)) (completeWithNoSharing x (d - 1))

complete x 0 = Empty

complete x d = Bin x child child
        where child = complete x (d - 1)

split n = (ceiling half, floor half)
        where half = (fromIntegral n) / 2

incomplete x 1 = Bin x Empty Empty
incomplete x 0 = Empty
incomplete x n | even numberOfChildrenLeft = Bin x evenChild evenChild
               | otherwise = Bin x (incomplete x first) (incomplete x second)
                    where numberOfChildrenLeft = n - 1
                          (first, second) = split numberOfChildrenLeft
                          evenChild = (incomplete x (div numberOfChildrenLeft 2))

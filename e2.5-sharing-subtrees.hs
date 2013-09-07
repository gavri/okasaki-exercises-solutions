data Tree a = Empty | Bin a (Tree a) (Tree a) deriving Show
completeWithNoSharing x 0 = Empty
completeWithNoSharing x d = Bin x (completeWithNoSharing x (d - 1)) (completeWithNoSharing x (d - 1))

complete x 0 = empty
        where empty = Empty

complete x d = Bin x child child
        where child = complete x (d - 1)

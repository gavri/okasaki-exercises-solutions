data Tree a = Empty | Bin a (Tree a) (Tree a) deriving Show

insert e Empty = Bin e Empty Empty
insert e tree@(Bin n l r) | e < n = Bin n (insert e l) r
                     | e > n = Bin n l (insert e r)
                     | otherwise = tree

input = (Bin 8 (Bin 5 (Bin 3 Empty Empty) (Bin 6 Empty Empty)) (Bin 10 (Bin 9 Empty Empty) (Bin 11 Empty (Bin 12 Empty Empty))))
output = insert 4 input
main = print $ output

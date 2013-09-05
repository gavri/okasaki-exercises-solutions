suffixes [] = [[]]
suffixes xss@(_:xs) = xss : suffixes xs
main = print $ suffixes [1, 2, 3, 4]

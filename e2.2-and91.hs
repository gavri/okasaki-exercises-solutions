import Test.HUnit

data Tree a = Empty | Bin a (Tree a) (Tree a)
member x Empty = False
member x (Bin n l r) | x < n = member x l
                   | x > n = member x r
                   | otherwise = True

quickerMember x Empty = False
quickerMember x (Bin n l r) = quickerMember' x (Bin n l r) n

quickerMember' x Empty possible = x == possible
quickerMember' x (Bin n l r) possible | x < n = quickerMember' x l possible
                                     | otherwise = quickerMember' x r n

input = (Bin 8 (Bin 5 (Bin 3 Empty Empty) (Bin 6 Empty Empty)) (Bin 10 (Bin 9 Empty Empty) (Bin 11 Empty (Bin 12 Empty Empty))))

main = runTestTT $ test [
  "member: on empty" ~: False ~=? member 8 Empty,
  "member: root exists" ~: True ~=? member 8 input,
  "member: internal node exists" ~: True ~=? member 11 input,
  "member: leaf exists" ~: True ~=? member 12 input,
  "member: does not exist" ~: False ~=? member 18 input,
  "quickerMember: on empty" ~: False ~=? quickerMember 8 Empty,
  "quickerMember: root exists" ~: True ~=? quickerMember 8 input,
  "quickerMember: internal node exists" ~: True ~=? quickerMember 11 input,
  "quickerMember: leaf exists" ~: True ~=? quickerMember 12 input,
  "quickerMember: does not exist" ~: False ~=? quickerMember 18 input
  ]

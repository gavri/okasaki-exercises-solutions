import Test.HUnit

suffixes [] = [[]]
suffixes xss@(_:xs) = xss : suffixes xs

main = runTestTT $ test $ [
  "Nonterminal" ~: [[1, 2, 3], [2, 3], [3], []] ~=? (suffixes [1, 2, 3]),
  "Terminal" ~: ([[]] :: [[Int]]) ~=? (suffixes [])
  ]

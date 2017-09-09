import Test.HUnit

(|>) a f = f a

data FiniteMap k v = FMEmpty | FMNode k v (FiniteMap k v) (FiniteMap k v) deriving (Show, Eq)

fmSet key value FMEmpty = FMNode key value FMEmpty FMEmpty
fmSet newKey newValue (FMNode key value left right) | (newKey < key) = (FMNode key value (fmSet newKey newValue left) right)
                                                    | (newKey > key) = (FMNode key value left (fmSet newKey newValue right))
                                                   | otherwise = (FMNode key newValue left right)

fmGet lookupKey FMEmpty = Nothing
fmGet lookupKey (FMNode key value left right) | (lookupKey < key) = fmGet lookupKey left
                                              | (lookupKey > key) = fmGet lookupKey right
                                              | otherwise = Just value

main = runTestTT $ test $ [
  "test" ~: FMNode 5 "Five" FMEmpty (FMNode 6 "Six" FMEmpty FMEmpty) ~=? FMEmpty |> (fmSet 5 "Five") |> (fmSet 6 "Six")
  ]

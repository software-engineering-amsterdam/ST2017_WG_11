f1, f2 :: Int -> Int
f1 = \ n -> sum [0..n]
f2 = \ n -> (n*(n+1)) / 2

test1 = quickCheckResult (\ n -> n >= 0 --> f1 n == f2 n)

--Not working
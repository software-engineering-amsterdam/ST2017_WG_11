


--Exercise 5
isDerangement xs ys = (isPermutation xs ys) && (foldl (&&) True (map (\(x,y) -> x /= y) (zip xs ys)))

deran :: Int -> [[Int]]
deran n = filter (\x -> isDerangement x set) (permutations set) where set = [0..n-1]

-- Tests for exercise 5
propDeranLength :: Ord a => [a] -> [a] -> Bool
propDeranLength a b = isDerangement a b --> length a == length b

propDeranPerm :: Ord a => [a] -> [a] -> Bool
propDeranPerm a b = isDerangement a b --> isPermutation a b

-- Only the empty list is a derangement of itself
propDeranRefl :: Ord a => [a] -> Bool
propDeranRefl a = isDerangement a a --> null a

propDeranSym :: Ord a => [a] -> [a] -> Bool
propDeranSym a b = isDerangement a b == isDerangement b a

propDeranTrans :: Ord a => [a] -> [a] -> [a] -> Bool
propDeranTrans a b c = ((isDerangement a b) && (isDerangement b c)) --> (isDerangement a c)

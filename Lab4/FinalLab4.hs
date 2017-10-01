



{--Task 4 --Time spent: 1hr so far

1. What exactly does xRx mean? Relation of x to x?
2. Is an empty set considered reflexive and irreflective to itself?
3.

-}









-- Exercise 8
-- Time spent: 1.5h

-- There is a difference between the symmetric closure of the transitive closure and the transitive closure of the symmetric closure. Here is an counterexample:
-- Let rel  = [(1,2),(2,3)]
-- trClos (symClos rel) == symClos ( trClos rel) => False
-- trClos (symClos rel) = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
-- symClos ( trClos rel) = [(1,2),(1,3),(2,1),(2,3),(3,1),(3,2)]
-- Both outcomes don't contain duplicated, and the first outcome is longer then the second. Thus there is a difference.




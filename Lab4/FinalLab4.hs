
{--Task 1 -- 3 Hours + 1.5 hours (manuel)


1. How would we do exercise 4.14 (pp. 129). Confused on what we're looking for and how to get it.
2. Exercise 4.21 pp 132
3. What is a powerset
4. How can russel paradox be used in programming? What are its uses.

-}


{--Task 4 --Time spent: 1hr so far + 1.5 hours (manuel)

1. What exactly does xRx mean? Relation of x to x?
2. Is an empty set considered reflexive and irreflective to itself?
3. I do understand the notion of a transitive closure, but the definition R+ = Un>=1 R^n (page 172) is unclear to me.
4. At Definition 5.75, the definition of a equivelence class is given, where they mention that R is the R-equivalence class of a, or the equivalence class of a modulo R. However, why is it the equivalence class of modulo R?
5.

-}


-- Exercise 8
-- Time spent: 1.5h

-- There is a difference between the symmetric closure of the transitive closure and the transitive closure of the symmetric closure. Here is an counterexample:
-- Let rel  = [(1,2),(2,3)]
-- trClos (symClos rel) == symClos ( trClos rel) => False
-- trClos (symClos rel) = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
-- symClos ( trClos rel) = [(1,2),(1,3),(2,1),(2,3),(3,1),(3,2)]
-- Both outcomes don't contain duplicated, and the first outcome is longer then the second. Thus there is a difference.




import Data.Vect

insSort_rhs_2 : (x : elem) -> (xs : Vect len elem) -> (xsStorted : Vect len elem) -> Vect (S len) elem

inSort : Vect n elem -> Vect n elem
inSort [] = []
inSort (x :: xs) = let xsStorted = inSort xs in
                       insSort_rhs_2 x xs xsStorted

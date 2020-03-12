import Data.Vect

insert : (x : elem) -> (xsStorted : Vect len elem) -> Vect (S len) elem
insert x [] = ?insert_rhs_1
insert x (y :: xs) = ?insert_rhs_2

inSort : Vect n elem -> Vect n elem
inSort [] = []
inSort (x :: xs) = let xsStorted = inSort xs in
                       insert x xsStorted

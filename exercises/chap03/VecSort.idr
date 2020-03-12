import Data.Vect

insert : Ord elem =>
         (x : elem) -> (xsStorted : Vect len elem) -> Vect (S len) elem

insert x [] = [x]
insert x (y :: xs) = ?insert_rhs_2

inSort : Ord elem => Vect n elem -> Vect n elem
inSort [] = []
inSort (x :: xs) = let xsStorted = inSort xs in
                       insert x xsStorted

import Data.Vect

insert : (x : elem) -> (xsStorted : Vect len elem) -> Vect (S len) elem

inSort : Vect n elem -> Vect n elem
inSort [] = []
inSort (x :: xs) = let xsStorted = inSort xs in
                       insert x xsStorted

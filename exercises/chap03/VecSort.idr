import Data.Vect

inSort : Vect n elem -> Vect n elem
inSort [] = []
inSort (x :: xs) = x :: xs

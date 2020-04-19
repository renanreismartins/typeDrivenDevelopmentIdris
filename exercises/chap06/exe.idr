import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix l c = Vect l (Vect c Int)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix l c = Vect l (Vect c Int)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]



TupleVect : Nat -> Type -> Type
TupleVect Z type = ()
TupleVect (S k) type = (type, (TupleVect k type))

test : TupleVect 4 Nat
test = (1,2,3,4,())

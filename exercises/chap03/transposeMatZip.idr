import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeMatZip : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMatZip [] = createEmpties
transposeMatZip (x :: xs) = let xsTrans = transposeMatZip xs in
                            zipWith (::) x xsTrans

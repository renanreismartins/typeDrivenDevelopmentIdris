import Data.Vect


addItem : Num a => Vect n a -> Vect n a -> Vect n a
addItem [] [] = []
addItem (x :: xs) (y :: ys) = x + y :: addItem xs ys

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = let summed = addMatrix xs ys in
                                    zipWith (+) x y :: summed
--addMatrix (x :: xs) (y :: ys) = let summed = addMatrix xs ys in
                                    --addItem x y :: summed
